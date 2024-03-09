{-# LANGUAGE OverloadedStrings #-}
module Insc.Seq (
  Seq(..),
  Content,
  Role(..),
  pruneSeq,
  degradeSeq,
  makeTrainDataset,
  makeDataset,
  readChatJson,
  encodeSeq,
  encodeSeqJson
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, encodeUtf8Builder)
import Data.Char (isSpace)
import Data.List (singleton)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BSL
import Crypto.Hash.MD5 (hash)
import Text.Hex (encodeHex)
import Data.ByteString.Builder (toLazyByteString)

data Role = SystemRole | UserRole | AssistantRole
          deriving (Show, Eq, Enum)

type Content = (Role, Text)

newtype Seq = Seq {
    contents :: [Content]
  } deriving Show

instance FromJSON Role where
  parseJSON (String v) = case v of
      "system"    -> return SystemRole
      "user"      -> return UserRole
      "assistant" -> return AssistantRole
      _           -> fail $ "unsupported role:" <> T.unpack v
  parseJSON _ = fail "expected String for role value"

instance {-# OVERLAPS #-} FromJSON Content where
  parseJSON (Object v) =
    (,) <$> v .: "role"
        <*> v .: "content"
  parseJSON _ = fail "expected Object for Content value"

instance FromJSON Seq where
  parseJSON v = Seq <$> parseJSON v

instance {-# OVERLAPS #-} ToJSON Content where
  toJSON (role, msg) =
    object ["role" .= roleName role, "content" .= msg]

instance ToJSON Seq where
  toJSON seq = toJSON $ contents seq

pruneSeq :: Seq -> Seq
pruneSeq (Seq xs) = Seq $ f <$> firstSys xs where
  firstSys ((AssistantRole, x) : xs) = (SystemRole, x) : xs
  firstSys xs = (SystemRole, "") : xs
  f (role, msg) = (role, pruneText msg)

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
mergeContents ((UserRole, a) : (UserRole, b) : xs) =
  (UserRole, a <> b) : mergeContents xs
mergeContents ((AssistantRole, a) : (AssistantRole, b) : xs) =
  (AssistantRole, a <> b) : mergeContents xs
mergeContents (x : xs) = x : mergeContents xs

-- | Break down multi-turn chat
degradeSeq :: Seq -> [Seq]
degradeSeq (Seq ctx) = Seq <$> brk [] (mergeContents ctx) where
  brk r [] = r
  brk r [_] = r
  brk _ ((SystemRole, a) : (UserRole, b) : (AssistantRole, c) : xs) =
    brk [[(SystemRole, a), (UserRole, b), (AssistantRole, c)]] xs
  brk r ((UserRole, a) : (AssistantRole, b) : xs) =
    brk ((head r <> [(UserRole, a), (AssistantRole, b)]) : r) xs
  brk _ _ = error "unexpected pattern"

-- | make train json dataset
makeTrainDataset :: [Seq] -> BSL.ByteString
makeTrainDataset seq = do
  let f (Seq a) = mconcat (g <$> mergeContents a) <> "<|endoftext|>"
      g a@(SystemRole, _) = im a
      g a = "\n" <> im a
      ds = object . singleton . ("text" .=) . f <$> seq
      im (role, msg) = "<|im_start|>" <> roleName role <> "\n" <> msg <> "<|im_end|>"
  encodeJson ds

roleName :: Role -> T.Text
roleName SystemRole = "system"
roleName UserRole = "user"
roleName AssistantRole = "assistant"

chatMD5 :: [Content] -> T.Text
chatMD5 c = encodeHex $ hash $ encodeUtf8 raw where
  raw = mconcat $ f <$> c
  f (role, msg) = roleName role <> msg

encodeUtf8Lazy :: T.Text -> BSL.ByteString
encodeUtf8Lazy = toLazyByteString . encodeUtf8Builder

makeDataset :: [(FilePath, [Seq])] -> BSL.ByteString
makeDataset xs = do
  let fIns :: (FilePath, [Seq]) -> [Value]
      fIns (path, seq) = chat path <$> seq
      chat file s = object [
        "file" .= file,
        "key" .= chatMD5 s.contents,
        "chat" .= mergeContents s.contents]
  encodeJson $ xs >>= fIns

encodeJson :: ToJSON a => a -> BSL.ByteString
encodeJson = (<> "\n") . encodePretty' (defConfig {confIndent = Spaces 2})

readChatJson :: FilePath -> IO Seq
readChatJson src = do
  json <- decodeFileStrict src
  case json of
    Nothing -> fail $ "load json failed:" <> src
    Just seq -> return seq

encodeSeq :: Seq -> BSL.ByteString
encodeSeq (Seq seq) = encodeUtf8Lazy content where
  content = "<s>\n" <> mconcat (f <$> seq) <> "</s>\n"
  f (SystemRole, a) = a <> "\n"
  f (UserRole, a) = "<ins>" <> g a <> "</ins>\n"
  f (AssistantRole, a) = a <> "\n"
  g s | length (T.lines s) <= 1 && T.length s <= 80 = s
      | otherwise = "\n" <> s <> "\n"

encodeSeqJson :: Seq -> BSL.ByteString
encodeSeqJson = encodeJson
