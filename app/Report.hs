module Report (report) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl', sortBy)
import Data.Bool (bool)
import System.FilePath (splitDirectories)
import Data.Function (on)
import qualified Algorithms.NaturalSort as NS

data Tree = Node {
  name :: FilePath,
  subtree :: Map FilePath Tree,
  count :: Int
} deriving Show

mktree :: FilePath -> Tree
mktree p = Node p mempty 0

subnodes :: Tree -> [Tree]
subnodes  = sortBy (NS.compare `on` name) . Map.elems . subtree

addPath :: Tree -> (FilePath, Int) -> Tree
addPath t (p, c) = f t p' where
  p' = splitDirectories p
  f a [] = a {count = a.count + c}
  f a (x:xs) = update a x $
    case Map.lookup x a.subtree of
      Just n -> f n xs
      Nothing -> f (mktree x) xs
  update a p n = a {subtree = Map.insert p n a.subtree}

buildTree :: [(FilePath, Int)] -> Tree
buildTree = foldl' addPath (mktree "")

data Report = Report {
  indent :: Int,
  title :: String,
  totalCount :: Int,
  selfCount :: Int
} deriving Show

reportTree :: Tree -> [Report]
reportTree = snd . f 0 where
  f :: Int -> Tree -> (Int, [Report])
  f i t = case Map.size t.subtree of
    0 -> (t.count, [Report i t.name t.count t.count])
    x -> if t.count > 0 || x > 1
      then let r0 = Report i t.name (t.count + c') t.count
               (c', r') = g $ f (i + 1) <$> subnodes t
           in  (r0.totalCount, [r0] <> r')
      else let h t' = t'{name = bool (t.name <> "/") "" (null t.name) <> t'.name }
           in  g $ f i . h <$> Map.elems t.subtree
  g [] = (0, [])
  g ((a, b):xs) = let (a', b') = g xs in (a + a', b <> b')

format :: [Report] -> String
format xs = unlines $ [header, split] <> (f <$> xs) where
  nw = max 40 $ maximum $ fnw <$> xs
  cw = max 4 $ maximum $ fcw <$> xs
  fnw r = r.indent * 2 + length r.title + 1
  fcw r = length (show r.totalCount ) + 1
  width = nw + cw
  padright p n s = let ns = length s in replicate (n - ns) p <> s
  padleft p n s = let ns = length s in s <> replicate (n - ns) p
  header = padleft ' ' nw "Path" <> padright ' ' cw "Record"
  split = replicate width '-'
  f :: Report -> String
  f r = let dir = r.totalCount /= r.selfCount
            pad = if dir then '.' else ' '
     in padleft pad nw (replicate (r.indent * 2) ' ' <> r.title <> " ")
     <> padright ' ' cw (show r.totalCount)

report :: [(FilePath, Int)] -> String
report = format . reportTree . buildTree
