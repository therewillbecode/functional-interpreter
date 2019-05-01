module Interpreter where

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

data Exp
  = Number Int
  | Variable String
  | Add Exp
        Exp
  | Mul Exp
        Exp
  deriving (Show)

eval :: Map String Int -> Exp -> Int
eval _ _ = 1
