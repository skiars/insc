{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Data.Maybe
import Data.List (sort)
import Control.Monad (when)
import qualified Data.ByteString.Lazy as BS
import System.FilePath.Glob
import System.Console.CmdArgs

import Insc

-- | load all datasets
globDataset :: Bool -> (FilePath -> Bool) -> FilePath -> IO [(FilePath, [Seq])]
globDataset verbose fil srcPath = do
  let f path = do
        xs <- readSeq path
        when verbose $
          putStrLn $ "load dataset " <> path <> " with " <> show (length xs) <> " records"
        return (path, xs)
  files <- sort . filter fil <$> globDir1 "**/*.ins" srcPath
  mapM f files

-- cmdargs
data ArgOpts = ArgOpts {
  format :: String,
  exclude :: [String],
  dump :: Maybe String,
  source :: FilePath,
  outout :: Maybe FilePath
  } deriving (Show, Data, Typeable)

argopts :: ArgOpts
argopts = ArgOpts {
  format = "default" &= help "The format of the exported dataset (support 'default' and 'chatml')." &= typ "TYPE",
  exclude = def &= help "The exclude file patterns." &= typ "[PATTERN]",
  dump = def &= help "Dump data to JSON or ins" &= typ "TYPE",
  source = def &= argPos 0 &= typ "PATH",
  outout = def &= help "The output file path." &= typFile
  }

dumpChat :: ArgOpts -> IO BS.ByteString
dumpChat args = do
  case fromJust args.dump of
    "ins" -> do
      chat <- readChatJson args.source
      return $ encodeSeq' chat
    "json" -> do
      chat <- readSeq args.source
      when (null chat) $
        fail $ "chat file is empty: " <> args.source
      return $ case chat of
        [s] -> encodeSeqJson s
        xs  -> encodeSeqJson' xs
    x -> error $ "unsupported dump type: " <> x

main :: IO ()
main = do
  args <- cmdArgs argopts
  let write = maybe BS.putStr BS.writeFile args.outout
      verbose = isJust args.outout
      exmatch x = not (any (flip match x . compile) args.exclude)
  content <- if isJust args.dump then do
    dumpChat args
  else case args.format of
    "default" -> do
      seq <- globDataset verbose exmatch args.source
      return $ makeDataset seq
    "chatml" -> do
      seq <- globDataset verbose exmatch args.source
      let seq' = degradeSeq =<< concatMap snd seq
      when verbose $
        putStrLn $ "collected " <> show (length seq') <> " records"
      return $ makeTrainDataset seq'
    x -> error $ "unknown export format: " <> x
  write content
