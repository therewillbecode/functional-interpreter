module Interpreter where

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

data Exp
  = Number Integer
  | Variable String
  | Add Exp
        Exp
  | Mul Exp
        Exp
  deriving (Show)

eval :: Map String Integer -> Exp -> Integer
eval _ _ = 1
