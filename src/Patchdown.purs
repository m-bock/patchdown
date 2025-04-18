module Patchdown where

import Prelude

import Control.Monad.Error.Class (liftEither, liftMaybe, try)
import Data.Argonaut.Core (Json)
import Data.Array ((!!))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags as RegFlags
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (error, message)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafeCrashWith)
import Patchdown.Converters.Purs (mkPursConverters)
import Patchdown.Converters.Raw (converterRaw)
import Patchdown.Common (Converter, codeBlock, printYaml, runConverter, yamlToJson)

type Opts =
  { filePath :: String
  , converters :: Array Converter
  }

replaceFn :: Map String Converter -> String -> Array (Maybe String) -> Effect String
replaceFn converterMap match matches = do
  converterName <- matches !! 0 # join # liftMaybe (error "converter name not found")
  yamlString <- matches !! 1 # join # liftMaybe (error "yaml string not found")

  converter <- Map.lookup converterName converterMap
    # liftMaybe (error ("converter not found: " <> converterName <> " " <> show (Map.keys converterMap)))

  json <- yamlToJson yamlString

  converted <- runConverter converter
    ( \{ decodeJson, convert } -> do
        r <- try do
          opts <- decodeJson json # lmap (\err -> error ("invalid json: " <> show err)) # liftEither
          convert { opts }

        pure
          ( case r of
              Left err -> codeBlock "text" ("🛑 ERROR at `" <> converterName <> "`\n" <> message err)
              Right result -> result
          )
    )

  let yamlPretty = printYaml json

  log (show matches)
  pure ("<!-- PATCH_START " <> converterName <> "\n" <> yamlPretty <> " -->" <> converted <> "<!-- END -->")


run :: Opts -> Effect Unit
run { filePath, converters } = do
  content <- readTextFile UTF8 filePath

  reg <- regex "<!-- PATCH_START ([a-zA-Z0-9_]+)\\s([\\s\\S]*?)-->[\\s\\S]*?<!-- END -->" RegFlags.global
    # lmap (\_ -> error "invalid regex")
    # liftEither

  let converterMap = converters # map (\c -> runConverter c _.name /\ c) # Map.fromFoldable

  patchedContent <- replaceEffect reg
    (replaceFn converterMap)
    content

  writeTextFile UTF8 filePath patchedContent

main :: Effect Unit
main = do
  filePath <- lookupEnv "PATCHDOWN_FILE_PATH"
    # map (fromMaybe' \_ -> unsafeCrashWith "PATCHDOWN_FILE_PATH not set")

  pursConverters <- mkPursConverters

  let converters = pursConverters <> [ converterRaw ]

  let
    opts =
      { filePath
      , converters
      }

  run opts

foreign import replaceEffectImpl
  :: (forall r. r -> Maybe r)
  -> (forall r. Maybe r)
  -> Regex
  -> (String -> Array (Maybe String) -> Effect String)
  -> String
  -> Effect String

replaceEffect :: Regex -> (String -> Array (Maybe String) -> Effect String) -> String -> Effect String
replaceEffect = replaceEffectImpl Just Nothing
