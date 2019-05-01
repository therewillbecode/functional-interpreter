module C where

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

-- Step 3:
data Exp
  = Number Int
  | Variable String
  | Add Exp
        Exp
  | Mul Exp
        Exp
  | Let String
        Exp
        Exp -- let name = exp in exp
  | Funcall Exp
            [Exp]
  | Lambda String
           Exp
  deriving (Show)

data Value
  = NumVal Int
  | FunVal (String -> Value)

instance Show Value where
  show (FunVal _) = "String -> Value"
  show a = show a

eval :: Map String Value -> Value
eval = undefined
--eval {} (Let "f" (Lambda "x" (Add (Variable x) (Variable x))) (FunCall (Variable "f") [Number 21])) -- 42
