module Main where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.Parser.Expression
import Text.Trifecta

main :: IO ()
main = loop env

loop :: Env -> IO ()
loop env = do
  putStr "calc> "
  s <- getLine
  case parseString (parseExp <* eof) mempty s of
    Success v -> loop1 (eval v env)
    Failure err -> print err >> loop env
  where
    loop1 (Nothing, e) = print "nothing" >> loop e
    loop1 (Just v, e) = print v >> loop e

type Variable = String

data Exp = NumberExp Double
         | VarExp Variable
         | AssignExp Variable Exp
         | PairExp Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Power Exp Exp
         | FuncCall String Exp
  deriving Show

skipSpaces :: Parser ()
skipSpaces = skipMany (oneOf " \t")

parseNumberExp :: Parser Exp
parseNumberExp = do
  v <- integerOrDouble
  return $
    NumberExp $
    case v of
      Left i -> fromInteger i
      Right d -> d

parseAssignExp :: Parser Exp
parseAssignExp = do
  var <- some letter
  skipSpaces
  char '='
  skipSpaces
  val <- parseExp
  return $ AssignExp var val

parseVarExp :: Parser Exp
parseVarExp = skipSpaces *> (VarExp <$> some letter) <* skipSpaces

parsePairExp :: Parser Exp
parsePairExp = parens $ parseExp >>= \e -> return (PairExp e)

parseTerm :: Parser Exp
parseTerm =
  parsePairExp <|> parseNumberExp <|> try parseAssignExp <|>
  try parseBuildInFunc <|>
  parseVarExp

buildInFuncMap :: Floating a => Map String (a -> a)
buildInFuncMap =
  M.fromList
    [ ("sin", sin)
    , ("cos", cos)
    , ("tan", tan)
    , ("asin", asin)
    , ("acos", acos)
    , ("atan", atan)
    , ("log", log)
    , ("exp", exp)
    , ("sqrt", sqrt)
    ]

parseBuildInFunc :: Parser Exp
parseBuildInFunc = do
  f <- choice $ map (\x -> string x) (M.keys buildInFuncMap)
  skipSpaces
  e <- parseExp
  return $ FuncCall f e

parseExp :: Parser Exp
parseExp = buildExpressionParser table parseTerm
  where
    table =
      [ [inOp '^' Power AssocLeft]
      , [inOp '*' Times AssocLeft, inOp '/' Div AssocLeft]
      , [inOp '+' Plus AssocLeft, inOp '-' Minus AssocLeft]
      ]
    inOp c exp assoc =
      Infix (skipSpaces >> char c >> skipSpaces >> return exp) assoc

type Env = Map Variable Double

env :: Env
env = M.empty

eval :: Exp -> Env -> (Maybe Double, Env)
eval (NumberExp d) env = (Just d, env)
eval (PairExp e) env = eval e env
eval (Plus e1 e2) env = (liftA2 (+) (fst (eval e1 env)) (fst (eval e2 env)), env)
eval (Minus e1 e2 ) env = (liftA2 (-) (fst (eval e1 env)) (fst (eval e2 env)), env)
eval (Times e1 e2) env = (liftA2 (*) (fst (eval e1 env)) (fst (eval e2 env)), env)
eval (Div e1 e2) env = (liftA2 (/) (fst (eval e1 env)) (fst (eval e2 env)), env)
eval (Power e1 e2) env =
  let n1 = fromJust (fst $ eval e1 env)
      n2 = doubleToMaybeInt $ fromJust (fst $ eval e2 env)
  in case n2 of
       Just n -> (Just (n1 ^ n), env)
       _ -> (Nothing, env)
eval (AssignExp var e) env =
  case exp of
    Nothing -> (Nothing, env)
    Just d -> (Just d, M.insert var d env)
  where
    exp = fst $ eval e env
eval (VarExp var) env =
  case M.lookup var env of
    Just val -> (Just val, env)
    _ -> (Nothing, env)
eval (FuncCall s e) env = (liftA f (fst res), (snd res))
  where
    f = fromJust $ M.lookup s buildInFuncMap
    res = eval e env

doubleToMaybeInt :: Double -> Maybe Integer
doubleToMaybeInt d =
  case fromInteger (round d) == d of
    True -> Just (round d)
    _ -> Nothing
