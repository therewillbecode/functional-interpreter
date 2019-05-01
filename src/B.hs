module B where

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

-- Step 2:
data Exp
  = Number Integer
  | Variable String
  | Add Exp
        Exp
  | Mul Exp
        Exp
  | Let String
        Exp
        Exp
  deriving (Show) -- let name = exp in exp

data Value
  = NumVal Integer
  | FunVal (String -> Value)

instance Show Value where
  show (FunVal _) = "String -> Value"
  show a = show a

eval :: Map String Value -> Value
eval = undefined
--eval {} (Let "x" 42 (Add (Variable x) (Number 23))) -- 65
