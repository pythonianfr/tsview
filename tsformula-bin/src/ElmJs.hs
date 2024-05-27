{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -DELM_JS="tsformula.js"   #-}

module ElmJs
  ( Env(..)
  , run
  , envParser
  ) where

import           Protolude

import           Options.Applicative

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.FileEmbed           (embedFile)
import           NeatInterpolation        (untrimming)
import           System.Environment       (lookupEnv)
import           System.IO                (hClose)
import           System.IO.Temp           (withSystemTempFile)
import           System.Process.Typed     (runProcess_, shell)

data Env =
  Env
    { elmModule :: Text
    , jsPath    :: Maybe FilePath
    , debugPath :: Maybe FilePath
    }
  deriving (Show)

run :: ToJSON flags => flags -> Env -> IO ()
run flags (Env {..}) = do
  jsContent <- probeJsPath >>= maybe defaultJs readFile <&> renderJs
  maybe mempty (flip writeFile jsContent) debugPath
  withSystemTempFile "run-elm-js" $ \path hdl -> do
    hClose hdl
    writeFile path jsContent
    runProcess_ $ shell $ "node " <> path
  where
    probeJsPath :: IO (Maybe FilePath)
    probeJsPath =
      if isJust jsPath
        then return jsPath
        else lookupEnv "ELM_JS"
    jsonFlags :: Text
    jsonFlags = decodeUtf8 $ toS $ encodePretty flags
    renderJs :: Text -> Text
    renderJs elmJs =
      [untrimming|
$elmJs

var app = module.exports
    .Elm.$elmModule.init({
        flags: $jsonFlags
    });

app.ports.log.subscribe((x) => { console.log(x) });
|]
    defaultJs :: IO Text
    defaultJs = return $ decodeUtf8 $ $(embedFile ELM_JS)

-- helper with optparse-applicative
envParser :: Text -> Parser Env
envParser elmModule =
  Env elmModule <$>
  (optional $
   strOption $
   long "js" <> metavar "ELM_JS" <> help "Path to JS file compiled by Elm") <*>
  (optional $
   strOption $
   long "dumpjs" <>
   metavar "DUMP_JS" <> help "Debug : write JS file as run by node")
