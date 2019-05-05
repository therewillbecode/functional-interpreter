module Main where

import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)

import Interpreter
import Text.Pretty.Simple

main :: IO ()
main = do
  pPrint program
  print $ runExcept $ runReaderT (eval program) emptyScope
  where
    initScope = Scope $ M.fromList [("x", NumVal 42), ("y", NumVal 23)]
    readBoundVariables = Add (Variable "x") (Variable "y")
    bindVariableExp = Let "x" (Number 42) (Add (Variable "x") (Number 23)) -- 65
    plusOne = (Lambda "plusOne" (Add (Variable "x") (Number 1)))
    compose =
      (Lambda
         "f"
         (Lambda
            "g"
            (Lambda
               "x"
               (FunCall (Variable "f") (FunCall (Variable "g") (Variable "x"))))))
    plusTwo =
      FunCall
        (FunCall (Variable "compose") (Variable "plusOne"))
        (Variable "plusOne")
    program =
      Let
        "compose"
        compose
        (Let
           "plusOne"
           plusOne
           (Let "plusTwo" plusTwo (FunCall (Variable "plusTwo") (Number 1))))
    applyLambdaExp =
      Let
        "f"
        (Lambda "x" (Add (Variable "x") (Number 1)))
        (FunCall (Variable "f") (Number 21))
