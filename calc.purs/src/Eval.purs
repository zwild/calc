module Eval where

import Prelude

import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Env (Env)
import Math (abs, ceil, exp, floor, log, round, sqrt)
import Type (Exp(..), Op(..))

data Error = VariableNotFound
           | FunNotFound

instance showError :: Show Error where
  show VariableNotFound = "Variable not found"
  show FunNotFound = "Function not found"

type Result = { env :: Env, res :: Number }

mkResult :: Env -> Number -> Result
mkResult = { env: _, res: _ }

evalOpExp :: Op -> Exp -> Exp -> Env -> Either Error Result
evalOpExp Plus e1 e2 env = lift2 (\x y -> mkResult env (x.res + y.res)) (eval e1 env) (eval e2 env)
evalOpExp Minus e1 e2 env = lift2 (\x y -> mkResult env (x.res - y.res)) (eval e1 env) (eval e2 env)
evalOpExp Times e1 e2 env = lift2 (\x y -> mkResult env (x.res * y.res)) (eval e1 env) (eval e2 env)
evalOpExp Div e1 e2 env = lift2 (\x y -> mkResult env (x.res / y.res)) (eval e1 env) (eval e2 env)

evalVarExp :: String -> Env -> Either Error Result
evalVarExp var env = case SM.lookup var env of
  Just n -> Right $ mkResult env n
  Nothing -> Left VariableNotFound

evalAssignExp :: String -> Exp -> Env -> Either Error Result
evalAssignExp var exp env = map (\r -> mkResult (SM.insert var r.res env) r.res) (eval exp env)

evalFunExp :: String -> Exp -> Env -> Either Error Result
evalFunExp fun e env = case SM.lookup fun buildInFuncs of
  Nothing -> Left FunNotFound
  Just f -> map (\r -> mkResult env (f r.res)) (eval e env)
  where
    buildInFuncs :: SM.StrMap (Number -> Number)
    buildInFuncs = SM.fromFoldable
      [ (Tuple "abs" abs)
      , (Tuple "ceil" ceil)
      , (Tuple "exp" exp)
      , (Tuple "floor" floor)
      , (Tuple "log" log)
      , (Tuple "round" round)
      , (Tuple "sqrt" sqrt)
      ]

eval :: Exp -> Env -> Either Error Result
eval (NumberExp num) env = Right $ mkResult env num
eval (OpExp op e1 e2) env = evalOpExp op e1 e2 env
eval (PairExp exp) env = eval exp env
eval (VarExp var) env = evalVarExp var env
eval (AssignExp var exp) env = evalAssignExp var exp env
eval (FunExp fun exp) env = evalFunExp fun exp env
