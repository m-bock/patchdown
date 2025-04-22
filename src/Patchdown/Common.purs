module Patchdown.Common where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (JsonCodec)
import Data.Exists (Exists, mkExists, runExists)
import Data.String (Pattern(..))
import Data.String as Str
import Effect (Effect)

newtype MkConverter a = MkConverter (ConverterFields a)

type ConverterFields a =
  { name :: String
  , description :: String
  , codecJson :: JsonCodec a
  , convert :: { opts :: a } -> Effect String
  }

type Converter = Exists MkConverter

mkConverter :: forall a. ConverterFields a -> Converter
mkConverter fields = mkExists $ MkConverter fields

runConverter :: forall r. Converter -> (forall a. ConverterFields a -> r) -> r
runConverter c f = runExists ((\(MkConverter fields) -> fields) >>> f) c

codeBlock :: String -> String -> String
codeBlock lang str = "\n```" <> lang <> "\n" <> str <> "\n```\n"


foreign import yamlToJson :: String -> Effect Json

foreign import printYaml :: Json -> String

print :: forall a. EncodeJson a => String -> a -> String
print str val = str <> "\n\n" <> printYaml (encodeJson val)

mdQuote :: String -> String
mdQuote str = str
  # Str.split (Pattern "\n")
  # map (\line -> "> " <> line)
  # Str.joinWith "\n"

mdBold :: String -> String
mdBold str = "**" <> str <> "**"

mdH5 :: String -> String
mdH5 str = "##### " <> str