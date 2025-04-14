module Patchdown.Converters.Purs where

import Prelude

import Control.Monad.Error.Class (liftMaybe, throwError)
import Data.Argonaut (class EncodeJson, encodeJson, stringifyWithIndent)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Filterable (filter)
import Data.Foldable (findMap, foldMap, for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show (class ShowRecordFields)
import Data.String as Str
import Data.Traversable (for)
import Effect (Effect)
import Effect.Exception as Exc
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Patchdown.Types (Converter, codeBlock, mkConverter, printYaml)
import Prim.Row (class Nub)
import Prim.RowList (class RowToList)
import PureScript.CST (RecoveredParserResult(..))
import PureScript.CST (parseModule) as CST
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Print as Print
import PureScript.CST.Range (class TokensOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types (Declaration(..), Foreign(..), Ident(..), Labeled(..), Module(..), ModuleBody(..), Name(..), Proper(..), SourceToken)

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
    ]

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
  , convert: \{ opts } -> convertPursType cache opts
  }

convertPursType :: Cache -> PursTypeOpts -> Effect String
convertPursType cache { filePath, ident } = do
  cst <- cache.getCst filePath

  let Module { body: ModuleBody { decls } } = cst

  decl <- decls
    # findMap
        ( \decl -> case decl of
            DeclType a@{ name: Name { name: Proper name } } b c | name == ident -> Just (DeclType a { keyword = removeLeadingComments a.keyword } b c)
            _ -> Nothing
        )
    # liftMaybe (Exc.error $ f "no type found" { ident })

  let ret = foldMap Print.printSourceToken (TokenList.toArray (tokensOf decl))

  pure $ codeBlock "purescript" ret

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
  , convert: \{ opts } -> convertVal cache opts
  }

convertVal :: Cache -> PursValOpts -> Effect String
convertVal cache { filePath, idents } = do
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
          # liftMaybe (Exc.error $ f "no value found" { ident, names })

        pure $ Str.joinWith ""
          [ maybe "" printTokens maySigCst # Str.trim
          , printTokens valCst
          ]
    )

  pure $ codeBlock "purescript" (Str.joinWith "\n\n" items)

printTokens :: forall a. TokensOf a => a -> String
printTokens cst = foldMap Print.printSourceToken (TokenList.toArray (tokensOf cst))

codecPursValOpts :: JsonCodec PursValOpts
codecPursValOpts = CAR.object "PursValOpts"
  { filePath: CA.string
  , idents: CA.array CA.string
  }

---

f :: forall a. EncodeJson a => String -> a -> String
f str val = str <> "\n\n" <> printYaml (encodeJson val)

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
  , convert: \{ opts } -> convertSig cache opts
  }

convertSig :: Cache -> PursSigOpts -> Effect String
convertSig cache { filePath, ident, dropIdent, dropType, moduleAlias, prefix } = do
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
    # liftMaybe (Exc.error $ f "no signature found" { ident, names })

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