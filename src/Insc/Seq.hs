{-# LANGUAGE OverloadedStrings #-}
module Insc.Seq (
  Seq(..),
  Content,
  Role(..),
  pruneSeq,
  stripSeq,
  makeChatMLDataset,
  makeDataset,
  readChatJson,
  encodeSeq,
  encodeSeq',
  encodeSeqJson,
  encodeSeqJson'
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, encodeUtf8Builder)
import Data.Char (isSpace)
import Data.List (intersperse, singleton)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Crypto.Hash.MD5 (hash)
import Text.Hex (encodeHex)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Data.Bifunctor

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

instance {-# OVERLAPS #-} FromJSON [Seq] where
  parseJSON v@(Array a) = case a' of
    x:_ -> case x of
      Array{} -> mapM parseJSON a'
      _       -> singleton <$> parseJSON v
    _   -> singleton <$> parseJSON v
    where
    a' = V.toList a
  parseJSON v = singleton <$> parseJSON v

instance {-# OVERLAPS #-} ToJSON Content where
  toJSON (role, msg) =
    object ["role" .= roleName role, "content" .= msg]

instance ToJSON Seq where
  toJSON seq = toJSON $ contents seq

pruneSeq :: Seq -> Seq
pruneSeq = mapSeq pruneText

stripSeq :: Seq -> Seq
stripSeq = mapSeq T.strip

data PruneState = Space | Newline | Newline2 | OtherChar
                deriving (Show, Eq)

mapSeq :: (Text -> Text) -> Seq -> Seq
mapSeq f (Seq xs) = Seq $ second f <$> firstSys xs where
  firstSys ((AssistantRole, x) : xs) = (SystemRole, x) : xs
  firstSys xs = (SystemRole, "") : xs

-- | Remove repeated spaces and newlines from text.
pruneText :: Text -> Text
pruneText = T.pack . reps OtherChar . T.unpack . T.strip where
  reps :: PruneState -> String -> String
  reps _ [] = []
  reps s ('`':'`':'`':xs) = let (c, xs') = code xs
                            in  "```" <> c <> reps s xs'
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
  code ""               = ("", "")
  code ('`':'`':'`':xs) = ("```", xs)
  code (x:xs)           = first (x:) $ code xs

mergeContents :: [Content] -> [Content]
mergeContents [] = []
mergeContents ((UserRole, a) : (UserRole, b) : xs) =
  (UserRole, a <> b) : mergeContents xs
mergeContents ((AssistantRole, a) : (AssistantRole, b) : xs) =
  (AssistantRole, a <> b) : mergeContents xs
mergeContents (x : xs) = x : mergeContents xs

-- | make ChatML json dataset
makeChatMLDataset :: [Seq] -> BSL.ByteString
makeChatMLDataset seq = do
  let f (Seq a) = mconcat (g <$> mergeContents a)
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
        "messages" .= mergeContents s.contents]
  encodeJson $ xs >>= fIns

encodeJson :: ToJSON a => a -> BSL.ByteString
encodeJson = (<> "\n") . encodePretty' (defConfig {confIndent = Spaces 2})

readChatJson :: Maybe FilePath -> IO [Seq]
readChatJson src = do
  json <- maybe (decodeStrict <$> BS.getContents) decodeFileStrict src
  case json of
    Nothing -> fail $ "load json failed: " <> fromMaybe "<stdin>" src
    Just seq -> return seq

encodeSeq :: Seq -> BSL.ByteString
encodeSeq (Seq seq) = encodeUtf8Lazy content where
  content = "<s>\n" <> mconcat (uncurry f <$> zip seq rseq) <> "</s>\n"
  rseq = SystemRole : (fst <$> seq)
  f (SystemRole, a) _             = a <> "\n"
  f (UserRole, a) _               = "<ins>" <> g a <> "</ins>\n"
  f (AssistantRole, a) SystemRole = "<res>" <> g a <> "</res>\n"
  f (AssistantRole, a) _          = a <> "\n"
  g s | length (T.lines s) <= 1 && T.length s <= 80 = s
      | otherwise = "\n" <> s <> "\n"

encodeSeq' :: [Seq] -> BSL.ByteString
encodeSeq' = mconcat . intersperse "\n" . (encodeSeq <$>)

encodeSeqJson :: Seq -> BSL.ByteString
encodeSeqJson = encodeJson

encodeSeqJson' :: [Seq] -> BSL.ByteString
encodeSeqJson' = encodeJson
