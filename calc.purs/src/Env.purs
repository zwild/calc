module Env where

import Data.StrMap (StrMap, empty)

type Env = StrMap Number

initEnv :: Env
initEnv = empty
