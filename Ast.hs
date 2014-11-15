{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Ast where

import qualified Control.Monad.Error as E

import Control.Monad.Error (Error, ErrorT)
import Data.Text (Text, pack)
import Data.Map (Map)


type Scope = [Map Ref Expr]
type Ref = String

data Statement
  = SExpr Expr
  | SDef  Def
  deriving Show

data Def = Def Ref Expr deriving Show

data Opt = Add | Sub | Div | Mul deriving (Show)

data Procedure
  = Lambda Ref Expr
  | Closure Scope Ref Expr
  deriving (Show)

data Expr 
  = XNil
  | XRef Ref
  | XDef Def
  | XBool Bool
  | XSeq [Expr]
  | XNum Double
  | XIf Expr Expr Expr
  | XApply Expr [Expr]
  | XProcedure Procedure
  | XOperator Opt
  deriving (Show)

data LispError
  = NameError String
  | TypeError
  | DivideByZero
  | InvalidInvoke Expr
  | OperatorError Opt
  | MsgError Text
  deriving Show

instance Error LispError where
  noMsg = MsgError "unknown error" 
  strMsg = MsgError . pack


lambda :: [Ref] -> Expr -> Expr
lambda = flip $ foldr (\p b -> XProcedure $ Lambda p b)



