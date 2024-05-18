{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Data.Maybe
import Data.List (sort)
import Control.Monad (when)
import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as BS
import System.FilePath.Glob
import System.Console.CmdArgs
import Control.Concurrent.ParallelIO (parallel)

import Insc
import qualified Report

-- cmdargs
data ArgOpts = ArgOpts {
  format :: String,
  exclude :: [String],
  report :: Bool,
  dump :: Maybe String,
  source :: Maybe FilePath,
  outout :: Maybe FilePath
  } deriving (Show, Data, Typeable)

isVorbose :: ArgOpts -> Bool
isVorbose args = isJust args.outout && not args.report

isReport :: ArgOpts -> Bool
isReport args = isJust args.outout && args.report

-- | load all datasets
globDataset :: ArgOpts -> (FilePath -> Bool) -> FilePath -> IO [(FilePath, [Seq])]
globDataset args fil srcPath = do
  let f path = do
        xs <- readSeq path
        when (isVorbose args) $
          putStrLn $ "load dataset " <> path <> " with " <> show (length xs) <> " records"
        return (path, xs)
  files <- sort . filter fil <$> globDir1 "**/*.ins" srcPath
  parallel $ f <$> files

argopts :: ArgOpts
argopts = ArgOpts {
  format = "default" &= help "The format of the exported dataset (support 'default' and 'chatml')." &= typ "TYPE",
  exclude = def &= help "The exclude file patterns." &= typ "[PATTERN]",
  dump = def &= help "Dump data to JSON or ins." &= typ "TYPE",
  source = def &= args &= typ "PATH",
  outout = def &= help "The output file path." &= typFile,
  report = def &= help "Output summary report."
  } &= program "insc"

dumpChat :: ArgOpts -> IO BS.ByteString
dumpChat args = do
  case fromJust args.dump of
    "ins" -> do
      chat <- readChatJson args.source
      return $ encodeSeq' chat
    "json" -> do
      chat <- readSeq' args.source
      when (null chat) $
        fail $ "chat file is empty: " <> fromMaybe "<stdin>" args.source
      return $ case chat of
        [s] -> encodeSeqJson s
        xs  -> encodeSeqJson' xs
    x -> error $ "unsupported dump type: " <> x

collect :: ArgOpts -> [(FilePath, [Seq])] -> IO [(FilePath, [Seq])]
collect args s = do
  let seq = concatMap snd s
      turns = length $ concatMap (filter f . contents) seq
      f = (== AssistantRole) . fst
  when (isReport args) $
    putStrLn $ Report.report $ second length <$> s
  when (isVorbose args || isReport args) $
    putStrLn $ "collected " <> show (length seq) <> " records (" <> show turns <> " turns)"
  return s

main :: IO ()
main = do
  args <- cmdArgs argopts
  let write = maybe BS.putStr BS.writeFile args.outout
      exmatch x = not (any (flip match x . compile) args.exclude)
      path = fromMaybe "" args.source

  content <- if isJust args.dump then do
    dumpChat args
  else case args.format of
    "default" -> do
      seq <- globDataset args exmatch $ fromMaybe "" args.source
      makeDataset <$> collect args seq
    "chatml" -> do
      seq <- globDataset args exmatch path
      makeChatMLDataset . concatMap snd <$> collect args seq
    x -> error $ "unknown export format: " <> x
  write content
