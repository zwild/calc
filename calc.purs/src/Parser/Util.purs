module Parser.Util where

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Int (fromString)
import Data.List ((:))
import Data.Maybe (fromJust)
import Data.String (singleton)
import Global (readFloat)
import Partial.Unsafe (unsafePartial)
import Prelude hiding (between)
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (between, many, many1, (<?>))
import Text.Parsing.StringParser.String (alphaNum, anyDigit, anyLetter, char, satisfy, string)

parens :: forall a. Parser a -> Parser a
parens = between (string "(") (string ")")

skipWhitespaces :: Parser Unit
skipWhitespaces = void $ many (satisfy \c -> c == '\t' || c == ' ')

integer :: Parser Int
integer = do
  ds <- many1 anyDigit
  (pure $ unsafePartial fromJust $ fromString $ foldMap singleton ds) <?> "integer"

double :: Parser Number
double = do
  ints <- many1 anyDigit
  _ <- char '.'
  fracs <- many1 anyDigit
  (pure $ readFloat $ foldMap singleton $ ints <> ('.' : fracs)) <?> "double"

integerOrDouble :: Parser (Either Int Number)
integerOrDouble = (try (Right <$> double) <|> (Left <$> integer)) <?> "integer or double"

variable :: Parser String
variable = do
  s <- char '_' <|> anyLetter
  e <- many alphaNum
  pure $ foldMap singleton (s : e)
