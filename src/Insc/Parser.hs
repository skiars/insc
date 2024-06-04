{-# LANGUAGE OverloadedStrings #-}
module Insc.Parser (
  parseIns
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Control.Arrow (left)
import Text.Megaparsec
import Text.Megaparsec.Char

import Insc.Seq

type Parser = Parsec Void Text

-- parse instruct file
unexp :: Parser a
unexp = string "<!--" *> fail "comments can only be toplevel tags."

comment :: Parser ()
comment = do
  _ <- string "<!--"
  _ <- manyTill anySingle (string "-->")
  space

tagged :: Text -> Role -> Parser Content
tagged tag role = do
  let openTag = "<" <> tag <> ">"
      closeTag = "</" <> tag <> ">"
  string openTag >> space
  (role,) . T.pack <$> manyTill anySingle (string closeTag)

tagIns :: Parser Content
tagIns = tagged "ins" UserRole

tagRes :: Parser Content
tagRes = tagged "res" AssistantRole

textElm :: Parser Content
textElm = (AssistantRole,) . T.pack <$> manyTill anySingle end where
  end = lookAhead $ try (newline >> tags) <|> unexp
  tags = string "<ins>" <|> string "</s>" <|> string "<res>"

tags :: Parser Content
tags = tagIns <|> tagRes

tagS :: Parser Seq
tagS = do
  _ <- many comment
  _ <- string "<s>"
  let content = space *> (tags <|> textElm)
  Seq <$> manyTill content (try $ newline >> string "</s>")

parseIns :: String -> Text -> Either String [Seq]
parseIns src x = left errorBundlePretty $ parse p src x where
  p = map pruneSeq <$> p1
  p1 = space *> many comment *> manyTill (tagS <* space) eof
