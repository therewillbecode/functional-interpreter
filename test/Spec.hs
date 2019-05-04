import Test.Hspec

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

import Control.Monad.Except
import Control.Monad.State

import Data.Either
import Prelude

import Interpreter

main :: IO ()
main = hspec step1

step1 =
  describe "Step 1" $
  it "Eval can read bound variables" $
  runExcept (evalStateT (eval expr) Scope) `shouldBe` Right (NumVal 65)
  where
    Scope = Scope $ M.fromList [("x", 42), ("y", 23)]
    expr = Add (Variable "x") (Variable "y")

step2 =
  describe "Step 2" $
  it "Eval can bind variables" $
  runExcept (evalStateT (eval expr) emptyScope) `shouldBe` Right (NumVal 65)
  where
    expr = Let "x" (Number 42) (Add (Variable "x") (Number 23))

step3 =
  describe "Step3" $
  it "Can apply lambdas" $
  runExcept (evalStateT (eval exp) emptyScope) `shouldBe` Right (NumVal 42)
  where
    exp =
      (Let
         "f"
         (Lambda "x" (Add (Variable "x") (Variable "x")))
         (FunCall (Variable "f") (Number 21)) -- 42
       )
