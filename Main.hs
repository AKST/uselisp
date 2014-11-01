{-# LANGUAGE LambdaCase #-}

import Prelude hiding (lookup)

import qualified Control.Monad.Identity as I
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Error as E
import qualified Control.Monad.State as S
import qualified Text.Parsec as P
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec (Parser, (<|>))
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (Reader, ReaderT)
import Control.Monad.Error (Error, ErrorT)
import Control.Monad.State (State, StateT, get, modify)
import Control.Monad.Fix (fix)
import Data.Text (Text)
import Data.Map (Map)

{--------------------------- TYPES ---------------------------}

type Ref = String

data Statement
  = SExpr Expr
  | SDefn Ref Expr
  deriving Show

data Expr 
  = XNil
  | XNum Int
  | XRef Ref
  | XApply Expr Expr
  | XLambda Ref Expr
  | XClosure Scope Ref Expr
  deriving (Show)

data LispError
  = NameError
  | InvokeError Expr
  | MsgError String
  deriving Show

instance Error LispError where
  noMsg = MsgError "unknown error" 
  strMsg = MsgError


{--------------------------- MAIN ---------------------------}


main = print "hello world"


{--------------------------- PARSER ---------------------------}


parser :: Text -> Parser Expr
parser text = undefined


{--------------------------- RUNTIME ---------------------------}


type Runtime a = StateT Global (ErrorT LispError Identity) a 
type Global = Map Ref Expr


runtime :: Statement -> Runtime Expr
runtime = \case
  SExpr expr -> runEval (eval expr) 
  SDefn name expr -> do
    evaled <- runEval (eval expr)
    modify $ Map.insert name evaled
    return XNil


runEval :: Eval a -> Runtime a
runEval eval = do
  globals <- get
  let env = Env [] globals 
      res = I.runIdentity $ E.runErrorT $ R.runReaderT eval env
  case res of
    Left error -> E.throwError error
    Right eval -> return eval


{--------------------------- EVALUATION ---------------------------}


type Eval a = ReaderT Env (ErrorT LispError Identity) a 
type Scope = [Map Ref Expr]

data Env = Env { scope :: Scope, global :: Global }
  deriving Show  


eval :: Expr -> Eval Expr
eval = \case 
  XRef ref -> lookup ref 
  XLambda p b -> do 
    Env env _ <- R.ask
    return (XClosure env p b)
  XApply fun arg -> do 
    fn <- eval fun
    ag <- eval arg   
    apply fn arg
  other -> return other


apply :: Expr -> Expr -> Eval Expr
apply invoked a = case invoked of
  (XClosure e p b) -> flip R.withReaderT (eval b) $ \r ->
    r { scope = Map.singleton p a : e }
  notAFunction -> E.throwError (InvokeError notAFunction) 


lookup :: Ref -> Eval Expr
lookup r = do
  Env env global <- R.ask
  flip fix env $ \f -> \case
    e:es 
      | Just v <- Map.lookup r e -> return v
      | Nothing <- Map.lookup r e -> f es
    []
      | Just v <- Map.lookup r global -> return v
      | Nothing <- Map.lookup r global -> E.throwError NameError
    
