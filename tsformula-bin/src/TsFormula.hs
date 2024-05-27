{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TsFormula where

import           Protolude

import qualified Data.Text        as DT

import           Data.Aeson       (ToJSON, Value)
import           System.Directory (doesFileExist)
import           System.FilePath  (takeExtension)

type ReturnType = Text

data RenderOption
  = Inspect
  | Format
  deriving (Show, Generic, ToJSON)

data Cmd
  = ShowSpec
  | Parse ReturnType (Maybe RenderOption) [Formula]
  | Edit ReturnType (Maybe RenderOption) [Formula]
  deriving (Show, Generic, ToJSON)

data Payload =
  Payload
    { cmd        :: Cmd
    , spec       :: Value
    , reduceSpec :: Bool
    }
  deriving (Show, Generic, ToJSON)

data Formula =
  Formula
    { name :: Text
    , code :: Text
    }
  deriving (Show, Generic, ToJSON)

loadFormulas :: [Text] -> IO [Formula]
loadFormulas xs =
  fmap mconcat $
  for xs $ \x ->
    let fpath = toS x
     in ifM (doesFileExist fpath) (fromFile fpath) (fromCmdLine x)
  where
    fromFile :: FilePath -> IO [Formula]
    fromFile fpath =
      if (takeExtension fpath == ".csv")
        then fmap fromCsvLine . drop 1 . lines <$> readFile fpath
        else pure . Formula "unamed_from_file" <$> readFile fpath
    fromCsvLine :: Text -> Formula
    fromCsvLine line =
      let (n, c) = second (DT.drop 1) $ DT.break (== ',') line
       in Formula n $ DT.dropAround (== '\'') c
    fromCmdLine :: Text -> IO [Formula]
    fromCmdLine x = return $ pure $ Formula "unamed_from_cmd_line" x

showSpec :: IO Cmd
showSpec = return ShowSpec

parse :: Text -> Maybe RenderOption -> [Text] -> IO Cmd
parse r mo xs = Parse r mo <$> loadFormulas xs

edit :: Text -> Maybe RenderOption -> [Text] -> IO Cmd
edit r mo xs = Edit r mo <$> loadFormulas xs
