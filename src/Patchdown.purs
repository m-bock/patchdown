module Patchdown
  ( main
  , run
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, liftEither, liftMaybe, throwError, try)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonDecodeError, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.String as Str
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags as RegFlags
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error, message)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafeCrashWith)
import Patchdown.Common (Converter, codeBlock, logInfo, mdH5, mdQuote, print, printYaml, runConverter, yamlToJson)
import Patchdown.Converters.Purs (mkConverterPurs)
import Patchdown.Converters.Raw (converterRaw)

type Opts =
  { filePath :: String
  , converters :: Array Converter
  }

mkStartTag :: String -> String
mkStartTag inner = "<!-- PD_START:" <> inner <> "-->"

endTag :: String
endTag = "<!-- PD_END -->"

replacePdSection
  :: String
  -> ( { converterName :: String, yamlStr :: String, enable :: Boolean }
       -> Effect { newYamlStr :: String, newContent :: String }
     )
  -> Effect String
replacePdSection content replaceFn = do
  let regexStr = mkStartTag "([a-zA-Z0-9_]*)(\\!?)\\s([\\s\\S]*?)" <> "[\\s\\S]*?" <> endTag

  reg <- regex regexStr RegFlags.global
    # lmap (\_ -> error "invalid regex")
    # liftEither

  replaceEffect reg
    ( \_ matches -> do
        { converterName, enable, yamlStr } <-
          case matches of
            [ Just converterName, Just exclam, Just yamlStr ] -> pure { converterName, enable: exclam /= "!", yamlStr }
            _ -> throwError (error $ print "invalid matches" { matches })

        { newContent, newYamlStr } <- replaceFn { converterName, yamlStr, enable }

        pure (mkStartTag (converterName <> (if enable then "" else "!") <> "\n" <> newYamlStr) <> newContent <> endTag)
    )
    content

data Err
  = InvalidYaml { err :: String }
  | InvalidOptions { json :: Json, err :: JsonDecodeError }
  | InvalidConverter { json :: Json }
  | ConverterError { newYamlStr :: String, err :: String }

mapErrEff :: forall e m a. MonadEffect m => MonadError e m => (Error -> e) -> Effect a -> m a
mapErrEff mpErr eff = do
  ret :: Either Error a <- liftEffect (try eff)
  case ret of
    Left err -> throwError (mpErr err)
    Right val -> pure val

tag :: String
tag = "Patchdown"

getReplacement
  :: Map String Converter
  -> { converterName :: String, yamlStr :: String, enable :: Boolean }
  -> ExceptT Err Effect { newYamlStr :: String, newContent :: String }
getReplacement converterMap { converterName, yamlStr, enable } = do
  json <- yamlToJson yamlStr # mapErrEff \err -> InvalidYaml { err: message err }

  converter <- Map.lookup converterName converterMap
    # liftMaybe (InvalidConverter { json })

  runConverter converter
    ( \{ codecJson, convert, name } -> do
        opts <- CA.decode codecJson json # lmap (\err -> InvalidOptions { json, err }) # liftEither

        let newYamlStr = printYaml $ CA.encode codecJson opts

        newContent <-
          if enable then do
            logInfo tag "run converter" { name }
            convert { opts } # mapErrEff (\err -> ConverterError { newYamlStr, err: message err })
          else do
            logInfo tag "skip converter" { name }
            pure ""

        pure
          { newContent, newYamlStr }
    )

run :: Opts -> Effect Unit
run { filePath, converters } = do
  let converterMap = converters # map (\c -> runConverter c _.name /\ c) # Map.fromFoldable
  let converterNames = Map.keys converterMap

  logInfo tag "start" { filePath, converterNames }

  fileContent <- readTextFile UTF8 filePath

  patchedFileContent <-
    replacePdSection fileContent \opts@{ converterName, yamlStr } -> do
      ret <- runExceptT $ getReplacement converterMap opts

      pure case ret of
        Left (InvalidYaml { err }) ->
          { newYamlStr: yamlStr
          , newContent: errorBox_ converterName err
          }
        Left (InvalidOptions { json, err }) ->
          { newYamlStr: printYaml json
          , newContent: errorBox_ converterName (printJsonDecodeError err)
          }
        Left (InvalidConverter { json }) ->
          { newYamlStr: printYaml json
          , newContent: errorBox converterName "Converter not found"
              { converterName
              , converterNames
              }
          }
        Left (ConverterError { newYamlStr, err }) ->
          { newYamlStr
          , newContent: errorBox_ converterName err
          }
        Right val -> val

  writeTextFile UTF8 filePath patchedFileContent

main :: Effect Unit
main = do
  filePath <- lookupEnv "PATCHDOWN_FILE_PATH"
    # map (fromMaybe' \_ -> unsafeCrashWith "PATCHDOWN_FILE_PATH not set")

  pursConverter <- mkConverterPurs

  let converters = [ pursConverter ] <> [ converterRaw ]

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

errorBox :: forall a. EncodeJson a => String -> String -> a -> String
errorBox sectionName message val = errorBoxImpl sectionName message (Just $ encodeJson val)

errorBox_ :: String -> String -> String
errorBox_ sectionName message = errorBoxImpl sectionName message Nothing

errorBoxImpl :: String -> String -> Maybe Json -> String
errorBoxImpl sectionName message val = wrapNl $ mdQuote $ Str.joinWith "\n"
  [ "<br>"
  , "🛑 Error at section `" <> sectionName <> "`"
  , ""
  , mdH5 message
  , case val of
      Just v -> codeBlock "yaml" (printYaml v)
      Nothing -> ""
  , "<br>"
  ]

wrapNl :: String -> String
wrapNl str = "\n" <> str <> "\n"