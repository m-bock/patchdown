module Patchdown.Converters.Purs
  ( mkPursConverters
  ) where

import Prelude

import Control.Monad.Error.Class (liftMaybe, throwError)
import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec, prismaticCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (hush)
import Data.Foldable (findMap, foldMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as Str
import Data.Traversable (for)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception as Exc
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Obj
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Patchdown.Common (Converter, codeBlock, mkConverter, printYaml)
import PureScript.CST (RecoveredParserResult(..))
import PureScript.CST (parseModule) as CST
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Print as Print
import PureScript.CST.Range (class TokensOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types (Declaration(..), Foreign(..), Ident(..), Label(..), Labeled(..), Module(..), ModuleBody(..), Name(..), Proper(..), Row(..), Separated(..), SourceToken, Type(..), Wrapped(..))

type Cache = { getCst :: String -> Effect (Module Void) }

mkCache :: Effect Cache
mkCache = do
  refCache <- Ref.new (Map.empty :: Map String (Module Void))
  pure
    { getCst: \filePath -> do
        cache <- Ref.read refCache
        case Map.lookup filePath cache of
          Just cst -> pure cst
          Nothing -> do
            content <- readTextFile UTF8 filePath
            cst <- getModuleCst content
            Ref.modify_ (Map.insert content cst) refCache
            pure cst
    }

mkPursConverters :: Effect (Array Converter)
mkPursConverters = do
  cache <- mkCache
  pure
    [ converterPursSig cache
    , converterPursVal cache
    , converterPursType cache
    , converterPursRecord cache
    ]

---

type PursRecordOpts =
  { filePath :: String
  , ident :: String
  , descriptions :: Object String
  }

converterPursRecord :: Cache -> Converter
converterPursRecord cache = mkConverter
  { name: "pursRecord"
  , description: "PureScript record converter"
  , decodeJson: CA.decode codecPursRecordOpts
  , convert: \{ opts: { filePath, ident, descriptions } } -> do
      cst <- cache.getCst filePath

      let decls = getDecls cst

      (cstHead /\ _ /\ cstType) <- decls
        # findMap
            ( \decl -> case decl of
                DeclType a@{ name: Name { name: Proper name } } b c | name == ident -> Just (a /\ b /\ c)
                _ -> Nothing
            )
        # liftMaybe (Exc.error $ print "no type found" { ident })

      pure case cstType of
        TypeRecord (Wrapped { value: Row { labels: Just (Separated { head, tail }) } }) ->
          let
            fields = ([ head ] <> map snd tail)
              # map
                  ( \(Labeled { label: Name { name: Label name }, value }) ->
                      { field: name
                      , typeSig: printTokens value
                      , description: Obj.lookup name descriptions
                      }
                  )
          in
            Str.joinWith "\n" $ join
              [ pure "\n"
              , pure ("#### `type " <> printTokens cstHead.name <> printTokens cstHead.vars <> "= {...}`")
              , mkTable (join $ map mkRow fields)
              ]
        _ -> "err"
  }

mkTable :: Array String -> Array String
mkTable children = join
  [ pure "<table>"
  , pure "  <tr>"
  , pure "    <th align='left'>Field</th>"
  , pure "    <th align='left'>Type</th>"
  , pure "    <th align='left'>Description</th>"
  , pure "  </tr>"
  , children
  , pure "</table>"
  ]

mkRow :: { field :: String, typeSig :: String, description :: Maybe String } -> Array String
mkRow { field, typeSig, description } = do
  [ "  <tr>"
  , "    <td valign='top'>"
  , "      <code>" <> field <> "</code>"
  , "    </td>"
  , "    <td valign='top'>"
  , "      <code>" <> typeSig <> "</code>"
  , "    </td>"
  , "    <td valign='top'>" <> fromMaybe "" description <> "</td>"
  , "  </tr>"
  ]

codecPursRecordOpts :: JsonCodec PursRecordOpts
codecPursRecordOpts = CAR.object "PursRecordOpts"
  { filePath: CA.string
  , ident: CA.string
  , descriptions: CAR.optionalWith (fromMaybe Obj.empty) Just (codecObj CA.string)
  }

codecObj :: forall a. JsonCodec a -> JsonCodec (Object a)
codecObj codec = prismaticCodec "obj" print g CA.jobject
  where
  print :: Object Json -> Maybe (Object a)
  print obj = for obj \j -> hush $ CA.decode codec j

  g :: Object a -> Object Json
  g = map (CA.encode codec)

---

type PursTypeOpts =
  { filePath :: String
  , ident :: String
  }

converterPursType :: Cache -> Converter
converterPursType cache = mkConverter
  { name: "pursType"
  , description: "PureScript type converter"
  , decodeJson: CA.decode codecPursTypeOpts
  , convert: \{ opts: { filePath, ident } } -> do
      cst <- cache.getCst filePath

      let Module { body: ModuleBody { decls } } = cst

      decl <- decls
        # findMap
            ( \decl -> case decl of
                DeclType a@{ name: Name { name: Proper name } } b c | name == ident -> Just (DeclType a { keyword = removeLeadingComments a.keyword } b c)
                _ -> Nothing
            )
        # liftMaybe (Exc.error $ print "no type found" { ident })

      let ret = foldMap Print.printSourceToken (TokenList.toArray (tokensOf decl))

      pure $ codeBlock "purescript" ret
  }

codecPursTypeOpts :: JsonCodec PursTypeOpts
codecPursTypeOpts = CAR.object "PursTypeOpts"
  { filePath: CA.string
  , ident: CA.string
  }

removeLeadingComments :: SourceToken -> SourceToken
removeLeadingComments all =
  all { leadingComments = [] }

---

type PursValOpts =
  { filePath :: String
  , idents :: Array String
  }

converterPursVal :: Cache -> Converter
converterPursVal cache = mkConverter
  { name: "pursVal"
  , description: "PureScript value converter"
  , decodeJson: CA.decode codecPursValOpts
  , convert: \{ opts: { filePath, idents } } -> do
      cst <- cache.getCst filePath

      let decls = getDecls cst

      items <- for idents
        ( \ident -> do

            let
              names = decls
                # Array.mapMaybe
                    ( \decl -> case decl of
                        DeclSignature (Labeled { label: Name { name: Ident name } }) -> Just name
                        _ -> Nothing
                    )

            let
              maySigCst = decls
                # findMap
                    ( case _ of
                        DeclSignature all@(Labeled { label: Name { name }, value }) | name == Ident ident -> Just (DeclSignature all)
                        _ -> Nothing
                    )

            valCst <- decls
              # findMap
                  ( \decl -> case decl of
                      DeclValue { name: Name { name: Ident name } } | name == ident -> Just decl
                      _ -> Nothing
                  )
              # liftMaybe (Exc.error $ print "no value found" { ident, names })

            pure $ Str.joinWith ""
              [ maybe "" printTokens maySigCst # Str.trim
              , printTokens valCst
              ]
        )

      pure $ codeBlock "purescript" (Str.joinWith "\n\n" items)
  }

codecPursValOpts :: JsonCodec PursValOpts
codecPursValOpts = CAR.object "PursValOpts"
  { filePath: CA.string
  , idents: CA.array CA.string
  }

---

print :: forall a. EncodeJson a => String -> a -> String
print str val = str <> "\n\n" <> printYaml (encodeJson val)

type PursSigOpts =
  { filePath :: String
  , ident :: String
  , dropIdent :: Maybe Boolean
  , dropType :: Maybe Boolean
  , moduleAlias :: Maybe String
  , prefix :: Maybe String
  }

converterPursSig :: Cache -> Converter
converterPursSig cache = mkConverter
  { name: "pursSig"
  , description: "PureScript signature converter"
  , decodeJson: CA.decode codecPursSigOpts
  , convert: \{ opts: { filePath, ident, dropIdent, dropType, moduleAlias, prefix } } -> do
      cst <- cache.getCst filePath

      let decls = getDecls cst

      let
        names = decls
          # Array.mapMaybe
              ( \decl -> case decl of
                  DeclSignature (Labeled { label: Name { name: Ident name } }) -> Just name
                  DeclForeign _ _ (ForeignValue (Labeled { label: Name { name: Ident name } })) -> Just name
                  _ -> Nothing
              )

      typeCst <- decls
        # findMap
            ( case _ of
                DeclSignature (Labeled { label: Name { name }, value }) | name == Ident ident -> Just value
                DeclForeign _ _ (ForeignValue (Labeled { label: Name { name }, value })) | name == Ident ident -> Just value
                _ -> Nothing
            )
        # liftMaybe (Exc.error $ print "no signature found" { ident, names })

      let typeStr = printTokens typeCst

      let dropIdent' = fromMaybe false dropIdent
      let dropType' = fromMaybe false dropType

      let
        ret = Str.joinWith ""
          [ if dropIdent' then ""
            else case moduleAlias of
              Just alias -> alias <> "."
              Nothing -> ""
          , if dropIdent' then "" else ident
          , if dropIdent' && dropType' then "" else " :: "
          , if dropType' then "" else typeStr
          ]

      pure $ Str.joinWith ""
        [ case prefix of
            Just p -> p
            Nothing -> ""
        , inTicks ret
        ]
  }

codecPursSigOpts :: JsonCodec PursSigOpts
codecPursSigOpts = CAR.object "PursSigOpts"
  { filePath: CA.string
  , ident: CA.string
  , dropIdent: CAR.optional CA.boolean
  , dropType: CAR.optional CA.boolean
  , moduleAlias: CAR.optional CA.string
  , prefix: CAR.optional CA.string
  }

---

getModuleCst :: String -> Effect (Module Void)
getModuleCst content = case CST.parseModule content of
  ParseSucceeded cst ->
    pure cst
  ParseSucceededWithErrors cst errors ->
    throwError $ Exc.error "success with errors"
  ParseFailed { error, position } ->
    throwError $ Exc.error $ printParseError error

inTicks :: String -> String
inTicks str = "`" <> str <> "`"

getDecls :: Module Void -> Array (Declaration Void)
getDecls (Module { body: ModuleBody { decls } }) = decls

printTokens :: forall a. TokensOf a => a -> String
printTokens cst = foldMap Print.printSourceToken (TokenList.toArray (tokensOf cst))