module Parser.Calc where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Int (toNumber)
import Parser.Util (integerOrDouble, parens, skipWhitespaces, variable)
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (fix)
import Text.Parsing.StringParser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.StringParser.String (char)
import Type (Exp(..), Op(..))

parseNumberExp :: Parser Exp
parseNumberExp = do
  v <- integerOrDouble
  pure $
    NumberExp $
    case v of
      Left i -> toNumber i
      Right n -> n

parsePairExp :: Parser Exp
parsePairExp = parens (PairExp <$> (fix \_ -> (skipWhitespaces *> parseExp <* skipWhitespaces)))

parseVarExp :: Parser Exp
parseVarExp = VarExp <$> variable

parseAssignExp :: Parser Exp
parseAssignExp = do
  var <- variable
  skipWhitespaces
  _ <- char '='
  skipWhitespaces
  val <- parseExp
  pure $ AssignExp var val

parseFunExp :: Parser Exp
parseFunExp = do
  fun <- variable
  _ <- char '('
  skipWhitespaces
  exp <- parseExp
  skipWhitespaces
  _ <- char ')'
  pure $ FunExp fun exp

parseTerm :: Parser Exp
parseTerm = fix \_ ->
                  parsePairExp
              <|> parseNumberExp
              <|> try parseAssignExp
              <|> try parseFunExp
              <|> parseVarExp

parseExp :: Parser Exp
parseExp = buildExprParser table (skipWhitespaces *> parseTerm <* skipWhitespaces)
  where
    table =
      [ [inOp '/' Div AssocLeft, inOp '*' Times AssocLeft]
      , [inOp '-' Minus AssocLeft, inOp '+' Plus AssocLeft]
      ]
    inOp c exp assoc = Infix (char c *> pure (OpExp exp)) assoc
