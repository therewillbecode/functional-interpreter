{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.Either
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Data.Maybe

import Debug.Trace

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
  | FunVal (Exp -> StateT Memory (Except Err) Value)

instance Show Value where
  show (FunVal _) = "FunVal String -> Value"
  show (NumVal n) = show n

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
 -- and that way, the change to the environment automatically only affects the evaluation of the body of the lambda, you don't have to reset the binding after

-- use Reader instead of State and use local to update the map with local bindings since our langugae is pure
-- So when evaluating an application, you'd use that local function to insert something into the Map of bound variables (r) while you evaluated the body of the lambda.
{-
% flip runReaderT [("x", 3)] do liftIO . print =<< asks (M.! "x"); local (M.insert "x" 42) do liftIO . print =<< asks (M.! "x");
yahb
Solonarv: 3; 42
Solonarv
% flip runReaderT [("x", 3)] do
   liftIO . print =<< asks (M.! "x");
    local (M.insert "x" 42) do 
         liftIO . print =<< asks (M.! "x") 
         liftIO . print =<< asks (M.! "x")
Solonarv: 3; 42; 3
-}
eval :: Exp -> StateT Memory (Except Err) Value
eval (Number n) = return $ NumVal n
eval (Variable name) = lookupVarBinding name
eval (Add a b) = binaryOp add a b
eval (Mul a b) = binaryOp mul a b
eval (Let varName varExp exp) = bindVar varName varExp >> eval exp
eval (Lambda paramName bodyExp) = defineLambda paramName bodyExp
eval (FunCall funcName [arg]) = do
  func <- eval funcName
  apply func arg

defineLambda :: String -> Exp -> StateT Memory (Except Err) Value
defineLambda paramName body =
  return $ FunVal $ \exp -> eval (Let paramName exp body)

apply :: Value -> Exp -> StateT Memory (Except Err) Value
apply (FunVal func) arg = func arg
apply val _ =
  throwError $
  "TypeError - Not a function: \n" ++ show val ++ " is not a function"

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
