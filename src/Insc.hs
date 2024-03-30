module Insc (
  module Insc.Seq,
  readSeq',
  readSeq
) where

import Insc.Seq
import Insc.Parser
import Data.Maybe (fromMaybe)

import qualified Data.Text.IO as TI

readSeq' :: Maybe FilePath -> IO [Seq]
readSeq' path = do
  let path' = fromMaybe "<stdin>" path
  content <- maybe TI.getContents TI.readFile path
  case parseIns path' content of
    Left bundle -> putStr bundle >> fail ("parse failed in: " <> path')
    Right xs -> do
      return xs

readSeq :: FilePath -> IO [Seq]
readSeq = readSeq' . Just
