{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Runtime where

import Prelude hiding (lookup, takeWhile)

import qualified Control.Monad.Identity as I
import qualified Control.Monad.Error as E
import qualified Control.Monad.State as S
import qualified Data.Map as Map

import Control.Monad.Identity (Identity)
import Control.Monad.Error (ErrorT)
import Control.Monad.State (StateT, get, modify, lift, void)
import Control.Monad (liftM)
import Data.Map (Map)

import Debug.Trace (trace)

import Eval
import Ast


runRuntime :: Global -> Runtime Expr -> Either String (Expr, Global)
runRuntime state runtime = 
  case I.runIdentity $ E.runErrorT (S.runStateT runtime state) of 
    Right result -> Right result
    Left error   -> Left (show error)


type Runtime a = StateT Global (ErrorT LispError Identity) a


runtime :: Statement -> Runtime Expr
runtime = \case
  SExpr expr -> runEval (eval expr)
  SDef (Def name expr) -> do
    evaled <- runEval (eval expr)
    modify $ Map.insert name evaled
    return XNil


runEval :: Eval a -> Runtime a
runEval eval = do
  globals <- get
  let env = Env [] globals 
      res = I.runIdentity $ E.runErrorT $ S.evalStateT eval env
  case res of
    Left error -> E.throwError error
    Right eval -> return eval

