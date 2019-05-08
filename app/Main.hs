module Main where

import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

import Interpreter
import Text.Pretty.Simple

main :: IO ()
main
 -- pPrint program
 = do
  print $ runExcept $ runReaderT (eval ite) initScope
  where
    initScope = Scope $ M.fromList [("x", NumVal 42), ("y", NumVal 23)]
    readBoundVariables = Add (Variable "x") (Variable "y")
    bindVariableExp = Let "x" (Number 42) (Add (Variable "x") (Number 23)) -- 65
    ite = IfThenElse (Boolean False) (Variable "y") (Variable "x")
