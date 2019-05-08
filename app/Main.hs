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
  print $ runExcept $ runReaderT (eval e) initScope
  where
    initScope = Scope $ M.fromList [("x", NumVal 42), ("y", NumVal 23)]
    readBoundVariables = Add (Variable "x") (Variable "y")
    bindVariableExp = Let "x" (Number 42) (Add (Variable "x") (Number 23)) -- 65
    ite = IfThenElse (Boolean False) (Variable "y") (Variable "x")
    e =
      Let
        "f"
        (Lambda "x" (Add (Variable "x") (Variable "x")))
        (FunCall (Variable "f") (Number 28)) -- 42
