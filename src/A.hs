module A where

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

-- Step 1:
data Exp
  = Number Integer
  | Variable String
  | Add Exp
        Exp
  | Mul Exp
        Exp
  deriving (Show)

eval :: Map String Integer -> Exp -> Integer
eval = undefined
-- test.hs
-- import TomsCode
-- let res = eval (Map.ofList [("x", 42), ("y", 23)]) (Add (Variable "x") (Variable "y")) -- 65
-- assert_eq res 65
