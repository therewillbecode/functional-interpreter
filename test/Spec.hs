import Test.Hspec

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

import Control.Monad.Except
import Control.Monad.State

import Data.Either
import Interpreter
import Prelude

main :: IO ()
main = hspec step1 >> hspec step2

step1 =
  describe "Step 1" $
  it "Eval can read bound variables" $
  runExcept (evalStateT (eval expr) memory) `shouldBe` Right 65
  where
    memory = Memory $ M.fromList [("x", 42), ("y", 23)]
    expr = Add (Variable "x") (Variable "y")

step2 =
  describe "Step 2" $
  it "Eval can bind variables" $
  runExcept (evalStateT (eval expr) emptyMemory) `shouldBe`
  (Right 65 :: Either Err Int)
  where
    expr = Let "x" (Number 42) (Add (Variable "x") (Number 23))
