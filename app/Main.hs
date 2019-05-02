module Main where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

import Interpreter

main :: IO ()
main = print $ runExcept $ evalStateT (eval expr) emptyMemory
  where
    expr =
      Let
        "f"
        (Lambda "x" (Add (Variable x) (Variable x)))
        (FunCall (Variable "f") [Number 21])
     -- 42
