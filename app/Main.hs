{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Data.Maybe
import Data.List (isInfixOf, sort)
import qualified Data.Text.IO as TI
import Control.Monad (when)
import System.FilePath.Glob
import System.Console.CmdArgs

import Insc

-- | load all datasets
globDataset :: (FilePath -> Bool) -> FilePath -> IO [(FilePath, [Seq])]
globDataset fil srcPath = do
  let f path = do
        xs <- readSeq path
        putStrLn $ "load dataset " <> path <> " with " <> show (length xs) <> " records"
        return (path, xs)
  files <- sort . filter fil <$> globDir1 "**/*.ins" srcPath
  mapM f files

globDataset' :: FilePath -> IO [(FilePath, [Seq])]
globDataset' = globDataset $ const True

-- cmdargs
data ArgOpts = ArgOpts {
  classify :: Bool,
  dump :: Maybe String,
  source :: String
  } deriving (Show, Data, Typeable)

argopts :: ArgOpts
argopts = ArgOpts {
  classify = def &= help "Make origin dataset for classification.",
  dump = def &= help "Dump data to JSON or ins" &= typ "TYPE",
  source = def &= argPos 0 &= typ "FILE"
  }

dumpChat :: ArgOpts -> IO ()
dumpChat args = do
  case fromJust args.dump of
    "ins" -> do
      chat <- readChatJson args.source
      TI.putStr $ encodeSeq chat
    "json" -> do
      chat <- readSeq $ args.source
      when (null chat) $
        fail $ "chat file is empty: " <> args.source 
      TI.putStr $ encodeSeqJson $ head chat
    x -> error $ "unsupported dump type: " <> x
  return ()

main :: IO ()
main = do
  args <- cmdArgs argopts
  if isJust args.dump then do
    dumpChat args
  else if args.classify then do
    seq <- globDataset (not . isInfixOf "tags/") "dataset"
    makeClassifyDataset "classify_dataset.json" seq
  else do
    seq <- globDataset' args.source
    let seq' = degradeSeq =<< concatMap snd seq
    putStrLn $ "collected " <> show (length seq') <> " records"
    makeTrainDataset "temp_dataset.json" seq'
