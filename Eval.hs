{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Eval where

import Prelude hiding (lookup)

import qualified Control.Monad.Error as E
import qualified Control.Monad.State as S

import qualified Data.Map as Map 

import Control.Monad.Identity (Identity)
import Control.Monad.Error (ErrorT)
import Control.Monad.State (StateT, get, modify, lift, void)
import Control.Monad.Fix (fix)
import Control.Monad (liftM)

import Debug.Trace (trace)

import Data.Text (Text, pack)
import Data.Map (Map) 

import Ast


type Global = Map Ref Expr

type Eval a = StateT Env (ErrorT LispError Identity) a 

data Env = Env { scope :: Scope, global :: Global }
  deriving Show  


eval :: Expr -> Eval Expr
eval = \case 
  XRef ref -> lookup ref 
  XProcedure (Lambda p b) -> do 
    Env env _ <- S.get
    return (XProcedure (Closure env p b))
  XApply func args -> do 
    func <- eval func
    args <- eval `mapM` args
    apply func args
  XIf cond cons altv ->
    eval cond >>= \case  
      XBool True  -> eval cons
      XBool False -> eval altv
      isntBoolean -> E.throwError TypeError
  XDef (Def name body) -> do
    newValue <- eval body
    modify $ \case 
      Env (s:os) g -> Env (Map.insert name newValue s:os) g
      Env [    ] g -> Env [Map.singleton name newValue] g
    return XNil
  XSeq exprs -> flip fix exprs $ 
    \f -> \case
      [    ] -> return XNil
      [expr] -> eval expr
      e:rest -> eval e >> f rest
  other -> return other


apply :: Expr -> [Expr] -> Eval Expr
apply invoked args = case invoked of 
  XOperator opt -> executeOpt opt args
  XProcedure pd -> iterate (XProcedure pd) args
    where 
      iterate result [] = return result
      iterate (XProcedure pd) args@(arg:rest) = do 
        result <- call pd arg
        iterate result rest 
      iterate notAFunction _ = 
        E.throwError $ InvalidInvoke notAFunction
  notAFunction -> 
    E.throwError $ InvalidInvoke notAFunction 


call :: Procedure -> Expr -> Eval Expr
call ps arg = case ps of
  Closure e p b -> flip S.withStateT (eval b) $ \r ->
    r { scope = Map.singleton p arg : e }
  Lambda p b -> flip S.withStateT (eval b) $ \r ->
    r { scope = [Map.singleton p arg] }


lookup :: Ref -> Eval Expr
lookup r = do
  Env env global <- S.get
  flip fix env $ \f -> \case
    e:es 
      | Just v <- Map.lookup r e -> return v
      | Nothing <- Map.lookup r e -> f es
    []
      | Just v <- Map.lookup r global -> return v
      | Nothing <- Map.lookup r global -> E.throwError $ NameError r
    

executeOpt :: Opt -> [Expr] -> Eval Expr
executeOpt opt args = case args of

  [XNum n] -> case opt of
    Div | n == 0 -> E.throwError DivideByZero
    Div -> return $ XNum (1/n)
    Sub -> return $ XNum (-n)
    _   -> return $ XNum n

  XNum n:rest -> iterate rest n
    where iterate [        ] acc = return $ XNum acc
          iterate (arg:rest) acc
            | XNum 0 <- arg, Div <- opt = E.throwError DivideByZero
            | XNum n <- arg = iterate rest $ case opt of
                Div -> acc / n
                Mul -> acc * n
                Add -> acc + n
                Sub -> acc - n
            | otherwise = E.throwError TypeError

  [] -> E.throwError $ OperatorError opt
  _  -> E.throwError TypeError


