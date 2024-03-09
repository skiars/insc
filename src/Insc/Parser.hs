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

tagIns :: Parser Content
tagIns = do
  char '<' >> string "ins>" >> space
  (UserRole,) . T.pack <$> manyTill anySingle (string "</") <* string "ins>"

textElm :: Parser Content
textElm = (AssistantRole,) . T.pack <$> manyTill anySingle (lookAhead $ string "<" <|> string "<ins>" <|> unexp)

tagS :: Parser Seq
tagS = do
  _ <- many comment
  _ <- string "<s>"
  let content = space *> (tagIns <|> textElm)
  Seq <$> manyTill content (string "</s>")

parseIns :: String -> Text -> Either String [Seq]
parseIns src x = left errorBundlePretty $ parse p src x where
  p = map pruneSeq <$> p1
  p1 = space *> many comment *> manyTill (tagS <* space) eof
