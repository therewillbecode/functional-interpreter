{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Interpreter where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Either
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe

import Prelude

data Exp a where
  Number :: Int -> Exp Int
  Boolean :: Bool -> Exp Bool
  Variable :: String -> Exp a
  Add :: Exp Int -> Exp Int -> Exp Int
  Mul :: Exp Int -> Exp Int -> Exp Int
  IfThenElse :: Exp Bool -> Exp a -> Exp a -> Exp a
  Or :: Exp Bool -> Exp Bool -> Exp Bool
  And :: Exp Bool -> Exp Bool -> Exp Bool
  Not :: Exp Bool -> Exp Bool
  Let :: String -> Exp a -> Exp b -> Exp c -- let name = exp in exp
  FunCall :: Exp String -> Exp a -> Exp a
  Lambda :: String -> Exp a -> Exp a

deriving instance Show (Exp a)

newtype EvalM a = EvalM
  { runEvalM :: Scope -> Either LangErr a
  } deriving (Functor, Applicative, Monad, MonadReader Scope)

data Value
  = NumVal Int
  | BoolVal Bool
  | FunVal (Value -> ReaderT Scope (Except LangErr) Value)

instance Show Value where
  show (FunVal _) = "FunVal String -> Value"
  show (NumVal n) = show n

unNumVal :: Value -> Int
unNumVal (NumVal n) = n
unNumVal x = error (show x ++ " is not a NumVal")

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
  | ExpectedBoolVal
  deriving (Show)

newtype Scope =
  Scope (Map String Value)
  deriving (Show)

emptyScope :: Scope
emptyScope = Scope M.empty

eval :: Exp a -> ReaderT Scope (Except LangErr) Value
eval (Number n) = return $ NumVal n
eval (Variable name) = lookupVarBinding name
eval (Boolean b) = return $ BoolVal b
--eval (And a b) = do
--  bA <- eval a
--  bB <- eval b
--  return $ binaryOp (&&) a b
--eval (Or a b) = do
--  (BoolVal a) <- eval a
--  (BoolVal b) <- eval b
--  return $ BoolVal $ a || b
--eval (Not a) = do
--  (BoolVal a) <- eval a
--  return $ BoolVal $ not a
eval (IfThenElse cond a b) = do
  condVal <- eval cond
  ifThenElse condVal a b
eval (Add a b) = binaryOp add a b
eval (Mul a b) = binaryOp mul a b
eval (Let varName varExp body) = do
  varVal <- eval varExp
  bindVar varName varVal (eval body)
eval (Lambda paramName bodyExp) = defineLambda paramName bodyExp
eval (FunCall funcName arg) = do
  func <- eval funcName
  apply func arg

defineLambda :: String -> Exp a -> ReaderT Scope (Except LangErr) Value
defineLambda paramName body = do
  currScope <- ask
  return $ FunVal $ \val -> bindVar paramName val (eval body)

apply :: Value -> Exp a -> ReaderT Scope (Except LangErr) Value
apply (FunVal func) arg = eval arg >>= \a -> func a
apply val _ = throwError $ LangErr (TypeError ExpectedFunction) (pure val)

bindVar ::
     String
  -> Value
  -> ReaderT Scope (Except LangErr) Value
  -> ReaderT Scope (Except LangErr) Value
bindVar name val = local (\(Scope m) -> Scope $ M.insert name val m)

binaryOp ::
     (Value -> Value -> Either LangErr Value)
  -> Exp a
  -> Exp a
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

add :: Value -> Value -> Either LangErr Value
add (NumVal a) (NumVal b) = Right $ NumVal $ a + b
add a (NumVal _) = Left $ LangErr (TypeError ExpectedNumVal) (pure a)
add (NumVal _) b = Left $ LangErr (TypeError ExpectedNumVal) (pure b)
add _ _ = Left $ LangErr (TypeError ExpectedNumVal) Nothing

mul :: Value -> Value -> Either LangErr Value
mul (NumVal a) (NumVal b) = Right $ NumVal $ a * b
mul a (NumVal _) = Left $ LangErr (TypeError ExpectedNumVal) (pure a)
mul (NumVal _) b = Left $ LangErr (TypeError ExpectedNumVal) (pure b)
mul _ _ = Left $ LangErr (TypeError ExpectedNumVal) Nothing

primFunc ::
     (Value -> Either LangErr Value)
  -> Exp a
  -> ReaderT Scope (Except LangErr) Value
primFunc f expA = do
  valA <- eval expA
  either throwError return (f valA)

primFunc2 ::
     (Value -> Value -> Either LangErr Value)
  -> Exp a
  -> Exp a
  -> ReaderT Scope (Except LangErr) Value
primFunc2 f expA expB = do
  valA <- eval expA
  primFunc (f valA) expB

neg :: Exp Int -> ReaderT Scope (Except LangErr) Value
neg =
  primFunc
    (\case
       NumVal i -> Right $ NumVal $ negate i
       a -> Left $ LangErr (TypeError ExpectedNumVal) (Just a))

compose :: Value -> Value -> ReaderT Scope (Except LangErr) Value
compose =
  \case
    a@(NumVal _) -> error $ show $ LangErr (TypeError ExpectedFunction) Nothing
    FunVal f ->
      \case
        b@(NumVal _) ->
          error $ show $ LangErr (TypeError ExpectedFunction) Nothing
        FunVal g -> return $ FunVal (f <=< g)

ifThenElse :: Value -> Exp a -> Exp a -> ReaderT Scope (Except LangErr) Value
ifThenElse (BoolVal bool') expA expB =
  if bool'
    then eval expA
    else eval expB
ifThenElse e _ _ = throwError (LangErr (TypeError ExpectedBoolVal) (pure e))
