{-# LANGUAGE LambdaCase #-}

module Interpreter where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.Either
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Data.Maybe

import Prelude

data Exp
  = Number Int
  | Variable String
  | Add Exp
        Exp
  | Mul Exp
        Exp
  | Let String
        Exp
        Exp -- let name = exp in exp
  | FunCall Exp
            [Exp]
  | Lambda String
           Exp
  deriving (Show)

data Value
  = NumVal Int
  | FunVal (String -> Value)

instance Show Value where
  show (FunVal _) = "FunVal String -> Value"
  show x = show x

type Err = String

{-
data LangErr = {
      unboundVar String Expr
      divBy0 String Expr
      typeMismatch String Expr
      NotFunction String String
      varAlreadyBound

instance Error LangError 

showError :: LangErr ->  String 

}
-}
newtype Memory =
  Memory (Map String Value)
  deriving (Show)

data VarBinding =
  VarBinding String
             Value
  deriving (Show)

emptyMemory :: Memory
emptyMemory = Memory M.empty

eval :: Exp -> StateT Memory (Except Err) Value
eval (Number n) = return $ NumVal n
eval (Variable name) = lookupVarBinding name
eval (Add a b) = binaryOp add a b
eval (Mul a b) = binaryOp mul a b
eval (Let varName varExp exp) = bindVar varName varExp >> eval exp
eval (Lambda var body) = lookupVarBinding var >> eval body

binaryOp ::
     (Value -> Value -> Either Err Value)
  -> Exp
  -> Exp
  -> StateT Memory (Except Err) Value
binaryOp op a b = do
  valA <- eval a
  valB <- eval b
  either throwError return (op valA valB)

lookupVarBinding :: String -> StateT Memory (Except Err) Value
lookupVarBinding name = do
  (Memory mem) <- get
  case M.lookup name mem of
    (Just varBinding) -> return varBinding
    Nothing -> throwError ("Unbound Variable: " ++ name)
  where
    mem = M.fromList [("x", 42), ("y", 23)]

bindVar :: String -> Exp -> StateT Memory (Except Err) VarBinding
bindVar name exp = do
  val <- eval exp
  modify (\(Memory m) -> Memory $ M.insert name val m)
  return $ VarBinding name val

getBindingVal :: VarBinding -> Value
getBindingVal (VarBinding _ val) = val

add :: Value -> Value -> Either Err Value
add (NumVal a) (NumVal b) = Right $ NumVal $ a + b
add _ _ = Left "typeError - Only NumVals can be added"

mul :: Value -> Value -> Either Err Value
mul (NumVal a) (NumVal b) = Right $ NumVal $ a * b
mul _ _ = Left "typeError - Only NumVals can be multiplied"
--Op (*) a b >>= \res -> return $ NumVal res
-- test.hs
-- import TomsCode
-- let res = eval (Map.ofList [("x", 42), ("y", 23)]) (Add (Variable "x") (Variable "y")) -- 65
-- assert_eq res 65
