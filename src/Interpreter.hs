{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Either
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
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
            Exp
  | Lambda String
           Exp
  deriving (Show)

data Value
  = NumVal Int
  | FunVal (Exp -> ReaderT Scope (Except LangErr) Value)

instance Show Value where
  show (FunVal _) = "FunVal String -> Value"
  show (NumVal n) = show n

data LangErr =
  LangErr Err
          (Maybe Value)

instance Show LangErr where
  show (LangErr err val) = show err ++ " in the expression: \n" ++ show val

data Err
  = UnboundVar String
  | TypeError TypeErr
  | VarAlreadyBound String
  deriving (Show)

data TypeErr
  = ExpectedFunction
  | ExpectedNumVal
  deriving (Show)

newtype Scope =
  Scope (Map String Value)
  deriving (Show)

emptyScope :: Scope
emptyScope = Scope M.empty

eval :: Exp -> ReaderT Scope (Except LangErr) Value
eval (Number n) = return $ NumVal n
eval (Variable name) = lookupVarBinding name
eval (Add a b) = binaryOp add a b
eval (Mul a b) = binaryOp mul a b
eval (Let varName varExp exp) = do
  val <- eval varExp
  bindVar varName val (eval exp)
eval (Lambda paramName bodyExp) = defineLambda paramName bodyExp
eval (FunCall funcName arg) = do
  func <- eval funcName
  apply func arg

defineLambda :: String -> Exp -> ReaderT Scope (Except LangErr) Value
defineLambda paramName body =
  return $ FunVal $ \exp -> eval (Let paramName exp body)

apply :: Value -> Exp -> ReaderT Scope (Except LangErr) Value
apply (FunVal func) arg = func arg
apply val _ = throwError $ LangErr (TypeError ExpectedFunction) (pure val)

binaryOp ::
     (Value -> Value -> Either LangErr Value)
  -> Exp
  -> Exp
  -> ReaderT Scope (Except LangErr) Value
binaryOp op a b = do
  valA <- eval a
  valB <- eval b
  either throwError return (op valA valB)

lookupVarBinding :: String -> ReaderT Scope (Except LangErr) Value
lookupVarBinding name = do
  (Scope mem) <- ask
  case M.lookup name mem of
    (Just varBinding) -> return varBinding
    Nothing -> throwError $ LangErr (UnboundVar name) Nothing

bindVar ::
     String
  -> Value
  -> ReaderT Scope (Except LangErr) Value
  -> ReaderT Scope (Except LangErr) Value
bindVar name val = local (\(Scope m) -> Scope $ M.insert name val m)

add :: Value -> Value -> Either LangErr Value
add (NumVal a) (NumVal b) = Right $ NumVal $ a + b
add a (NumVal _) = Left $ LangErr (TypeError ExpectedNumVal) (pure a)
add (NumVal _) b = Left $ LangErr (TypeError ExpectedNumVal) (pure b)

mul :: Value -> Value -> Either LangErr Value
mul (NumVal a) (NumVal b) = Right $ NumVal $ a * b
mul a (NumVal _) = Left $ LangErr (TypeError ExpectedNumVal) (pure a)
mul (NumVal _) b = Left $ LangErr (TypeError ExpectedNumVal) (pure b)
