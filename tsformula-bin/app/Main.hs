{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Protolude

import           Options.Applicative

import           Data.Aeson               (eitherDecodeFileStrict)
import           NeatInterpolation        (trimming)
import qualified Options.Applicative.Help as Help
import           System.Environment       (lookupEnv)

import qualified ElmJs
import qualified TsFormula                as TS

sHelp :: Maybe Help.Doc
sHelp =
  pure . Help.extractChunk . Help.stringChunk . toS $
  [trimming|
Running Elm code from command line.

Enjoy !
    |]

data Env =
  Env
    { specPath   :: FilePath
    , specReduce :: Bool
    , jsEnv      :: ElmJs.Env
    }
  deriving (Show)

runJs :: IO TS.Cmd -> Env -> IO ()
runJs ioCmd (Env {..}) = do
  cmd <- ioCmd
  jsonSpec <- either (die . toS) return =<< eitherDecodeFileStrict specPath
  ElmJs.run (TS.Payload cmd jsonSpec specReduce) jsEnv

runParser :: Maybe FilePath -> IO ()
runParser envSpecPath = do
  res <-
    execParser $ info (withEnv cmds <**> helper) (fullDesc <> progDescDoc sHelp)
  uncurry runJs res
  where
    withEnv p = liftA2 (,) p envParser
    envParser :: Parser Env
    envParser =
      Env <$>
      (strOption $
       short 's' <>
       long "spec" <>
       maybe (noArgError $ ErrorMsg "No spec provided") value envSpecPath <>
       metavar "TS_SPEC" <> help "Path to JSON specification file") <*>
      (flag True False $ long "no-reduce" <> help "Do not reduce spec unions") <*>
      ElmJs.envParser "FormulaParserValidation.Main"
    returnType =
      strOption $
      short 'r' <>
      long "return" <>
      value "Series" <> metavar "TS_RETURN" <> help "Formula ReturnType"
    inspect =
      flag' TS.Inspect $
      short 'i' <> long "inspect" <> help "Render TypedOperator as text"
    format =
      flag' TS.Format $
      short 'f' <> long "format" <> help "Format TypedOperator to formula"
    formulaSrc =
      some $
      strArgument $
      metavar "formulas.csv | formula.txt | code, ..." <>
      (help $
       "A CSV formula file with name and code columns, " <>
       "formula code in a file or an inline code.")
    cmds :: Parser (IO TS.Cmd)
    cmds =
      hsubparser $
      mconcat
        [ command "show" $ info (pure TS.showSpec) fullDesc
        , command "parse" $
          info
            (TS.parse <$> returnType <*> optional inspect <*> formulaSrc)
            fullDesc
        , command "edit" $
          info
            (TS.edit <$> returnType <*> optional (inspect <|> format) <*>
             formulaSrc)
            fullDesc
        ]

main :: IO ()
main = join $ runParser <$> lookupEnv "TS_SPEC"
