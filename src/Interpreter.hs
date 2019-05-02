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
  deriving (Show)

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
  Memory (Map String Int)
  deriving (Show)

data VarBinding =
  VarBinding String
             Int
  deriving (Show)

eval :: Exp -> StateT Memory (Except Err) Int
eval exp@(Add a b) = add a b
eval exp@(Mul a b) = mul a b

emptyMemory :: Memory
emptyMemory = Memory M.empty

lookupVarBinding :: String -> StateT Memory (Except Err) Int
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

getBindingVal :: VarBinding -> Int
getBindingVal (VarBinding _ val) = val

--lookupVarVal :: String -> StateT Memory (Except Err) (Either Err Int)
--lookupVarVal name = return $ (gets (lookupVarBinding name))
binaryOp :: (Int -> Int -> Int) -> Exp -> Exp -> StateT Memory (Except Err) Int
binaryOp op (Variable nameA) (Variable nameB) = do
  a <- lookupVarBinding nameA
  b <- lookupVarBinding nameB
  return $ a `op` b
binaryOp op (Variable nameA) (Number b) = do
  a <- lookupVarBinding nameA
  return $ a `op` b
binaryOp op (Number a) (Variable nameB) = do
  b <- lookupVarBinding nameB
  return $ a `op` b
binaryOp op (Number a) (Number b) = return $ a `op` b

add :: Exp -> Exp -> StateT Memory (Except Err) Int
add = binaryOp (+)

mul :: Exp -> Exp -> StateT Memory (Except Err) Int
mul = binaryOp (*)
-- test.hs
-- import TomsCode
-- let res = eval (Map.ofList [("x", 42), ("y", 23)]) (Add (Variable "x") (Variable "y")) -- 65
-- assert_eq res 65
