module Lib.Parsing (skipRestOfLine, word, wordsP, linesOf, spaces, readText, number) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Char (isSpace)
import Data.Text (Text)
import Prelude hiding (words)
import qualified Data.Text as T

skipRestOfLine :: Parser ()
skipRestOfLine = P.skipWhile (not . P.isEndOfLine) >> (P.endOfLine <|> P.endOfInput)

spaces :: Parser [Char]
spaces = P.many1 (P.char ' ')

word :: Parser Text
word = P.takeWhile (not . isSpace)

wordsP :: Parser [Text]
wordsP = word `P.sepBy` spaces

number :: Parser Int
number = read <$> P.many1 P.digit

linesOf :: Parser a -> Parser [a]
linesOf parser = parser `P.sepBy` P.endOfLine

readText :: Read a => Text -> a
readText = read . T.unpack
