module Insc (
  module Insc.Seq,
  readSeq
) where

import Insc.Seq
import Insc.Parser

import qualified Data.Text.IO as TI

readSeq :: FilePath -> IO [Seq]
readSeq path = do
  content <- TI.readFile path
  case parseIns path content of
    Left bundle -> putStr bundle >> fail ("parse failed in: " <> path)
    Right xs -> do
      return xs
