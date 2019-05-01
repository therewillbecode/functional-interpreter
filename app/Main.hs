module Main where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

import A

main :: IO ()
main = print $ runExcept $ evalStateT (eval expr) emptyMemory
  where
    expr = Add (VariableName 3) (Number 5)