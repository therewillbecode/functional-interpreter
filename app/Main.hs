module Main where

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

import Interpreter

main :: IO ()
main = print $ eval M.empty expr
  where
    expr = Number 3
