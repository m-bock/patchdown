module Patchdown.Converters.Raw where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Patchdown.Common (Converter, ConvertResult, mkConverter)

type Opts =
  { filePath :: String
  }

converterRaw :: Converter
converterRaw = mkConverter
  { name: "raw"
  , description: "Raw converter"
  , codecJson: codecOpts
  , printOpts: show
  , convert: \{ opts } -> convert opts
  }

convert :: Opts -> Effect ConvertResult
convert { filePath } = do
  content <- readTextFile UTF8 filePath
  pure { content, errors: [] }

codecOpts :: JsonCodec Opts
codecOpts = CAR.object "Opts"
  { filePath: CA.string
  }