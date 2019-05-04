{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

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
newtype Scope =
  Scope (Map String Value)
  deriving (Show)

data VarBinding =
  VarBinding String
             Value
  deriving (Show)

emptyScope :: Scope
emptyScope = Scope M.empty
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
eval :: Exp -> ReaderT Scope (Except LangErr) Value
eval (Number n) = return $ NumVal n
eval (Variable name) = lookupVarBinding name
eval (Add a b) = binaryOp add a b
eval (Mul a b) = binaryOp mul a b
eval (Let varName varExp exp) = do
  val <- eval varExp
  bindVar varName val
  eval exp
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

bindVar :: String -> Value -> ReaderT Scope (Except LangErr) VarBinding
bindVar name val =
  local
    (\(Scope m) -> Scope $ M.insert name val m)
    (return $ VarBinding name val)

getBindingVal :: VarBinding -> Value
getBindingVal (VarBinding _ val) = val

add :: Value -> Value -> Either LangErr Value
add (NumVal a) (NumVal b) = Right $ NumVal $ a + b
add a (NumVal _) = Left $ LangErr (TypeError ExpectedNumVal) (pure a)
add (NumVal _) b = Left $ LangErr (TypeError ExpectedNumVal) (pure b)

mul :: Value -> Value -> Either LangErr Value
mul (NumVal a) (NumVal b) = Right $ NumVal $ a * b
mul a (NumVal _) = Left $ LangErr (TypeError ExpectedNumVal) (pure a)
mul (NumVal _) b = Left $ LangErr (TypeError ExpectedNumVal) (pure b)
