module Patchdown.Converters.Purs where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Codec (Codec, Codec')
import Data.Codec.Argonaut (JPropCodec, JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Sum as CAS
import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.String as Str
import Data.String.Extra (snakeCase)
import Data.Symbol (class IsSymbol)
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Exception as Exc
import Effect.Ref as Ref
import Foreign.Object (Object)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as NodeFS
import Patchdown.Common (Converter, codeBlock, mkConverter)
import Prim.Row (class Cons, class Nub, class Union)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors as CSTErr
import PureScript.CST.Print as Print
import PureScript.CST.Range (class TokensOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types as CST
import Record as Record
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type Cache = { getCst :: String -> Effect (Array Source) }

inTicks :: String -> String
inTicks str = "`" <> str <> "`"

getModuleCst :: String -> Effect (CST.Module Void)
getModuleCst content = case parseModule content of
  ParseSucceeded cst ->
    pure cst
  ParseSucceededWithErrors cst errors ->
    throwError $ Exc.error "success with errors"
  ParseFailed { error, position } ->
    throwError $ Exc.error $ CSTErr.printParseError error

printTokens :: forall a. TokensOf a => a -> String
printTokens cst = foldMap Print.printSourceToken (TokenList.toArray (tokensOf cst))

mkCache :: Effect Cache
mkCache = do
  refCache <- Ref.new (Map.empty :: Map String (Array Source))
  pure
    { getCst: \filePath -> do
        cache <- Ref.read refCache
        case Map.lookup filePath cache of
          Just sources -> pure sources
          Nothing -> do
            content <- NodeFS.readTextFile UTF8 filePath
            cst <- getModuleCst content
            let sources = getSources cst
            Ref.modify_ (Map.insert content sources) refCache
            pure sources
    }

type Opts =
  { filePath :: Maybe String
  , inline :: Boolean
  , split :: Boolean
  , pick :: Array PickItem
  }

type PickItem =
  { filePath :: Maybe String
  , prefix :: Maybe String
  , pick :: Pick
  }

data Pick
  = PickImport
      { moduleName :: Maybe String }
  | PickData
      { name :: String }
  | PickNewtype
      { name :: String }
  | PickType
      { name :: String }
  | PickSignature
      { name :: String }
  | PickForeign
      { name :: String }
  | PickValue
      { name :: String }

  | PickExtraTypeRecord
      { name :: String }

  -- | PickExtraDataOrNewtype
  --     { name :: String }
  | PickExtraSignatureOrForeign
      { name :: String }
  | PickExtraValueAndSignature
      { name :: String }
  | PickExtraAny
      { name :: String
      }

derive instance Eq Pick

-- PickModuleHeader
-- PickClass { name :: String, filePath :: Maybe String }
-- PickInstancechain
-- PickInstance
-- PickDerived
-- PickKindSignature
-- PickFixity
-- PickRole

data Source
  = SrcImport (CST.ImportDecl Void)
  | SrcDecl (CST.Declaration Void)

getSources :: CST.Module Void -> Array Source
getSources
  ( CST.Module
      { header: CST.ModuleHeader { imports }
      , body: CST.ModuleBody { decls }
      }
  ) = map SrcImport imports <> map SrcDecl decls

matchOnePick :: Pick -> Source -> Array String
matchOnePick pick decl = case pick of
  PickImport _ -> []
  PickData { name } -> [] -- TODO
  PickNewtype { name } -> [] -- TODO
  PickType { name } -> [] -- TODO
  PickSignature { name: name1 } -> case decl of
    SrcDecl all@(CST.DeclSignature (CST.Labeled { label: CST.Name { name: CST.Ident name2 } }))
      | name1 == name2 -> [ printTokens all ]
    _ -> []
  PickForeign { name } -> [] -- TODO
  PickValue { name } -> [] -- TODO

  PickExtraTypeRecord { name } -> [] -- TODO

  PickExtraValueAndSignature { name } ->
    matchManyPicks
      [ PickValue { name }
      , PickSignature { name }
      ]
      decl

  PickExtraSignatureOrForeign { name } ->
    matchManyPicks
      [ PickSignature { name }
      , PickForeign { name }
      ]
      decl

  PickExtraAny { name } ->
    matchManyPicks
      [ PickImport { moduleName: Just name }
      , PickData { name }
      , PickNewtype { name }
      , PickType { name }
      , PickSignature { name }
      , PickForeign { name }
      , PickValue { name }
      ]
      decl

matchManyPicks :: Array Pick -> Source -> Array String
matchManyPicks picks decl = foldMap (\p -> matchOnePick p decl) picks

derive instance Generic Pick _

mkConverterPurs :: Effect Converter
mkConverterPurs = do
  cache <- mkCache
  pure $ converterPurs cache

converterPurs :: Cache -> Converter
converterPurs cache = mkConverter
  { name: "purs"
  , description: "PureScript identifier converter"
  , codecJson: codecOpts
  , convert: convert cache
  }

getWrapFn :: Opts -> { wrapInner :: String -> String, wrapOuter :: String -> String }
getWrapFn { split, inline } =
  let
    wrapFn = if inline then inTicks else codeBlock "purescript"
  in
    { wrapInner: if split then identity else wrapFn
    , wrapOuter: if split then wrapFn else identity
    }

convert :: Cache -> { opts :: Opts } -> Effect String
convert cache { opts: opts@{ pick } } = do
  let { wrapInner, wrapOuter } = getWrapFn opts

  items :: (Array String) <- join <$> for pick
    ( \{ pick, filePath, prefix } -> do
        sources <- cache.getCst (fromMaybe "src/Main.purs" (filePath <|> opts.filePath))
        let results = sources >>= matchOnePick pick

        let
          addPrefix val = case prefix of
            Just p -> (p <> val)
            Nothing -> val

        pure $ map (addPrefix <<< wrapInner) results
    )

  pure $ wrapOuter (Str.joinWith "\n\n" items)

--- Codecs

codecOpts :: JsonCodec Opts
codecOpts = CA.object "Opts" $
  CAR.record
    { filePath: CAR.optional CA.string
    , pick: CAR.optional (oneOrMany codecPickItem)
    , inline: CAR.optional CA.boolean
    , split: CAR.optional CA.boolean
    }
    # fieldWithDefault @"split" false
    # fieldWithDefault @"inline" false
    # fieldWithDefault @"pick" []

mkPickItem :: Pick -> PickItem
mkPickItem pick =
  { filePath: Nothing
  , prefix: Nothing
  , pick
  }

codecPickItem :: JsonCodec PickItem
codecPickItem = CA.codec' dec enc
  where
  dec j =
    ( do
        pick <- CA.decode codecPick j
        pure $ mkPickItem pick
    ) <|> CA.decode codecCanonical j

  enc val =
    if val == mkPickItem val.pick then
      CA.encode codecPick val.pick
    else
      CA.encode codecCanonical val

  codecCanonical =
    CAR.object "PickItem"
      { filePath: CAR.optional CA.string
      , pick: codecPick
      , prefix: CAR.optional CA.string
      }

codecPick :: JsonCodec Pick
codecPick = CA.codec' dec enc
  where
  can :: JsonCodec Pick
  can = CAS.sumFlatWith
    CAS.defaultFlatEncoding
      { mapTag = replace (Pattern "Pick") (Replacement "") >>> snakeCase
      }
    "Pick"
    { "PickImport": CAR.record
        { moduleName: CAR.optional CA.string
        }
    , "PickData": CAR.record
        { name: CA.string
        }
    , "PickNewtype": CAR.record
        { name: CA.string
        }
    , "PickType": CAR.record
        { name: CA.string
        }
    , "PickSignature": CAR.record
        { name: CA.string
        }
    , "PickForeign": CAR.record
        { name: CA.string
        }
    , "PickValue": CAR.record
        { name: CA.string
        }
    , "PickExtraTypeRecord": CAR.record
        { name: CA.string
        }
    , "PickExtraSignatureOrForeign": CAR.record
        { name: CA.string
        }
    , "PickExtraValueAndSignature": CAR.record
        { name: CA.string
        }
    , "PickExtraAny": CAR.record
        { name: CA.string
        }
    }

  enc = case _ of
    PickExtraAny { name } -> CA.encode CA.string name
    other -> CA.encode can other

  dec j =
    ( do
        name <- CA.decode CA.string j
        pure (PickExtraAny { name })
    ) <|> CA.decode can j

--- Utils

altDec :: forall a. (Json -> Either JsonDecodeError a) -> JsonCodec a -> JsonCodec a
altDec dec c = CA.codec' dec' enc
  where
  dec' :: Json -> Either JsonDecodeError a
  dec' j = dec j <|> CA.decode c j

  enc :: a -> Json
  enc = CA.encode c

oneOrMany :: forall a. JsonCodec a -> JsonCodec (Array a)
oneOrMany c = altDec (map Array.singleton <<< CA.decode c) (CA.array c)

sequentialCodec :: forall r1 r2 r. Union r1 r2 r => Union r2 r1 r => Nub r r => JPropCodec (Record r1) -> (Record r1 -> JPropCodec (Record r2)) -> JPropCodec (Record r)
sequentialCodec c1 mkc2 = CA.codec dec enc
  where
  dec :: Object Json -> Either JsonDecodeError (Record r)
  dec obj = do
    ret1 :: Record r1 <- CA.decode c1 obj
    let c2 = mkc2 ret1
    ret2 :: Record r2 <- CA.decode c2 obj
    pure (Record.merge ret1 ret2)

  enc :: Record r -> List (String /\ Json)
  enc r =
    let
      ret1 :: List (String /\ Json)
      ret1 = CA.encode c1 (pickFields r)

      c2 :: JPropCodec (Record r2)
      c2 = mkc2 (pickFields r)

      ret2 :: List (String /\ Json)
      ret2 = CA.encode c2 (pickFields r)
    in
      ret1 <> ret2

pickFields :: forall r1 r2 t. Union r2 t r1 => Record r1 -> Record r2
pickFields = unsafeCoerce

---

fieldWithDefaultSparse
  :: forall @sym m x r r' t a b
   . IsSymbol sym
  => Monad m
  => Cons sym (Maybe x) t r
  => Cons sym x t r'
  => x
  -> (x -> Boolean)
  -> Codec m a b (Record r) (Record r)
  -> Codec m a b (Record r') (Record r')
fieldWithDefaultSparse defDec shouldNotEncode = fieldDimap @sym
  (\val -> if shouldNotEncode val then Nothing else Just val)
  (fromMaybe defDec)

fieldWithDefault
  :: forall @sym m x r r' t a b
   . IsSymbol sym
  => Monad m
  => Cons sym (Maybe x) t r
  => Cons sym x t r'
  => x
  -> Codec m a b (Record r) (Record r)
  -> Codec m a b (Record r') (Record r')
fieldWithDefault defDec = fieldDimap @sym Just (fromMaybe defDec)

fieldOptionalChatty
  :: forall @sym m x r t a b
   . IsSymbol sym
  => Monad m
  => Cons sym (Maybe x) t r
  => x
  -> Codec m a b (Record r) (Record r)
  -> Codec m a b (Record r) (Record r)
fieldOptionalChatty defEnc = fieldDimap @sym (fromMaybe defEnc >>> Just) identity

fieldDimap
  :: forall @sym m x y r r' t a b
   . IsSymbol sym
  => Monad m
  => Cons sym x t r
  => Cons sym y t r'
  => (y -> x)
  -> (x -> y)
  -> Codec m a b (Record r) (Record r)
  -> Codec m a b (Record r') (Record r')
fieldDimap f1 f2 codec = fieldCompose @sym (CA.codec' (f2 >>> pure) f1) codec

fieldCompose
  :: forall @sym m x y r r' t a b
   . Monad m
  => IsSymbol sym
  => Cons sym x t r
  => Cons sym y t r'
  => Codec' m x y
  -> Codec m a b (Record r) (Record r)
  -> Codec m a b (Record r') (Record r')
fieldCompose codec1 codec2 = CA.codec dec enc
  where
  prx = Proxy :: Proxy sym

  dec :: a -> m (Record r')
  dec j = do
    rec :: Record r <- CA.decode codec2 j
    let val = Record.get prx rec :: x
    val' :: y <- CA.decode codec1 val
    let rec' = Record.set prx val' rec :: Record r'
    pure rec'

  enc :: Record r' -> b
  enc r =
    let
      rec = Record.modify prx (CA.encode codec1) r :: Record r
    in
      CA.encode codec2 rec
