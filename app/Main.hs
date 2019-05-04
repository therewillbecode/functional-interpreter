module Main where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

import Interpreter

main :: IO ()
main = print $ runExcept $ evalStateT (eval applyLambdaExp) emptyMemory
  where
    readBoundVariables = Add (Variable "x") (Variable "y")
    bindVariableExp = Let "x" (Number 42) (Add (Variable "x") (Number 23)) -- 65
    applyLambdaExp =
      Let
        "f"
        (Lambda "x" (Add (Variable "x") (Variable "x")))
        (FunCall (Variable "f") (Number 21))
     -- 42
