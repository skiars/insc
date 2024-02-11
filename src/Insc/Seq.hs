{-# LANGUAGE OverloadedStrings #-}
module Insc.Seq (
  Seq(..),
  Content(..),
  pruneSeq,
  degradeSeq,
  makeTrainDataset,
  makeClassifyDataset,
  readChatJson,
  encodeSeq,
  encodeSeqJson
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Char (isSpace)
import Data.List (singleton)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BS

data Content = SysRole Text
             | InstRole Text
             | OutRole Text
             deriving Show

newtype Seq = Seq {
    contents :: [Content]
  } deriving Show

instance FromJSON Content where
  parseJSON (Object v) = do
    role <- v .: "role"
    text <- v .: "text"
    case role of
      "system"    -> return $ SysRole text
      "user"      -> return $ InstRole text
      "assistant" -> return $ OutRole text
      _           -> fail $ "unsupported role:" <> role
  parseJSON _ = fail "expected Object for Content value"

instance FromJSON Seq where
  parseJSON v = Seq <$> parseJSON v

instance ToJSON Content where
  toJSON msg =
    let im :: Text -> Text -> Value
        im role msg = object ["role" .= role, "text" .= msg]
    in case msg of
      SysRole a  -> im "system" a
      InstRole a -> im "user" a
      OutRole a  -> im "assistant" a

instance ToJSON Seq where
  toJSON seq = toJSON $ contents seq

pruneSeq :: Seq -> Seq
pruneSeq (Seq xs) = Seq $ f <$> firstSys xs where
  firstSys (OutRole x : xs) = SysRole x : xs
  firstSys xs = SysRole "" : xs
  f (SysRole x) = SysRole $ pruneText x
  f (InstRole x) = InstRole $ pruneText x
  f (OutRole x) = OutRole $ pruneText x

data PruneState = Space | Newline | Newline2 | OtherChar
                deriving (Show, Eq)

-- | Remove repeated spaces and newlines from text.
pruneText :: Text -> Text
pruneText = T.pack . reps OtherChar . T.unpack . T.strip where
  reps :: PruneState -> String -> String
  reps _ [] = []
  reps s (x:xs)
    | x == '\n'      = reps (selNl Newline2 Newline s) xs
    | isSpace x      = reps (selNl s Space s) xs
    | s /= OtherChar = p s <> (x : reps OtherChar xs)
    | otherwise      = x : reps OtherChar xs
  selNl b c a = if a == Newline || a == Newline2 then b else c
  p Space     = " "
  p Newline   = "\n"
  p Newline2  = "\n\n"
  p OtherChar = ""

mergeContents :: [Content] -> [Content]
mergeContents [] = []
mergeContents (InstRole a : InstRole b : xs) = InstRole (a <> b) : mergeContents xs
mergeContents (OutRole a : OutRole b : xs) = InstRole (a <> b) : mergeContents xs
mergeContents (x : xs) = x : mergeContents xs

-- | Break down multi-turn chat
degradeSeq :: Seq -> [Seq]
degradeSeq (Seq ctx) = Seq <$> brk [] (mergeContents ctx) where
  brk r [] = r
  brk r [_] = r
  brk _ (SysRole a : InstRole b : OutRole c : xs) =
    brk [[SysRole a, InstRole b, OutRole c]] xs
  brk r (InstRole a : OutRole b : xs) =
    brk ((head r <> [InstRole a, OutRole b]) : r) xs
  brk _ _ = error "unexpected pattern"

-- | make train json dataset
makeTrainDataset :: FilePath -> [Seq] -> IO ()
makeTrainDataset outFile seq = do
  let f (Seq a) = mconcat (g <$> mergeContents a) <> "<|endoftext|>"
      g (SysRole a) = im "system" a
      g (InstRole a) = "\n" <> im "user" a
      g (OutRole a) = "\n" <> im "assistant" a
      ds = object . singleton . ("text" .=) . f <$> seq
      im role msg = "<|im_start|>" <> role <> "\n" <> msg <> "<|im_end|>"
  encodeJsonFile outFile ds

makeClassifyDataset :: FilePath -> [(FilePath, [Seq])] -> IO ()
makeClassifyDataset outFile xs = do
  let fIns :: (FilePath, [Seq]) -> Value
      fIns (path, seq) = object ["file" .= path, "session" .= (chat <$> seq)]
      chat s = object ["chat" .= mergeContents s.contents]
  encodeJsonFile outFile $ fIns <$> xs

encodeJson :: ToJSON a => a -> BS.ByteString
encodeJson = (<> "\n") . encodePretty' (defConfig {confIndent = Spaces 2})

encodeJsonFile :: ToJSON a => FilePath -> a -> IO ()
encodeJsonFile path a = BS.writeFile path $ encodeJson a

readChatJson :: FilePath -> IO Seq
readChatJson src = do
  json <- decodeFileStrict src
  case json of
    Nothing -> fail $ "load json failed:" <> src
    Just seq -> return seq

encodeSeq :: Seq -> Text
encodeSeq (Seq seq) = "<s>\n" <> mconcat (f <$> seq) <> "</s>\n" where
  f (SysRole a) = a <> "\n"
  f (InstRole a) = "<ins>" <> a <> "</ins>\n"
  f (OutRole a) = a <> "\n"

encodeSeqJson :: Seq -> Text
encodeSeqJson = decodeUtf8 . BS.toStrict . encodeJson
