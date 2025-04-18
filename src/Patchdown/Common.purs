module Patchdown.Common where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (JsonDecodeError)
import Data.Either (Either)
import Data.Exists (Exists, mkExists, runExists)
import Effect (Effect)

newtype MkConverter a = MkConverter (ConverterFields a)

type ConverterFields a =
  { name :: String
  , description :: String
  , decodeJson :: Json -> Either JsonDecodeError a
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
