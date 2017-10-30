module Type where

import Prelude

type Variable = String

data Exp = NumberExp Number
         | OpExp Op Exp Exp
         | PairExp Exp
         | VarExp Variable
         | AssignExp Variable Exp
         | FunExp String Exp

instance showExp :: Show Exp where
  show (NumberExp n) = show n
  show (OpExp op e1 e2) = show e1 <> " " <> show op <> " " <> show e2
  show (PairExp e) = "(" <> show e <> show ")"
  show (VarExp v) = v
  show (AssignExp v e) = v <> " = " <> show e
  show (FunExp s e) = s <> "(" <> show e <> ")"

data Op = Plus
        | Minus
        | Times
        | Div

instance showOp :: Show Op where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"
