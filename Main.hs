{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import Prelude hiding (lookup, takeWhile)

import qualified Text.ParserCombinators.Parsec as P
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Error as E
import qualified Control.Monad.State as S
import qualified Text.Parsec as Parsec
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec.Combinator (choice, eof, manyTill)
import Text.ParserCombinators.Parsec.Char (letter, digit, char, oneOf, spaces, newline, anyChar)
import Text.ParserCombinators.Parsec.Prim (many)
import Text.ParserCombinators.Parsec (Parser, (<|>), (<?>), sepBy, sepEndBy)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (Reader, ReaderT)
import Control.Monad.Error (Error, ErrorT)
import Control.Monad.State (State, StateT, get, modify, lift, void)
import Control.Monad.Fix (fix)
import Control.Monad (liftM)
import Control.Applicative ((*>), (<*), (<$>), (<*>)) 
import Data.List.Utils (replace)
import Data.Text (Text, pack)
import Data.Map (Map)

import Debug.Trace (trace)

import System.IO (hFlush, stdout)

{--------------------------- TYPES ---------------------------}

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


{--------------------------- MAIN ---------------------------}


type App a = StateT AppState IO a

data AppState = AppState { buffer :: String, runtimeEnv :: Global }

main = runApp app 

app = do
  input <- readInput

  if input == "" then
    lift $ putStrLn " Ciao\n"
  else if complete input then
    display >> app
  else 
    app


readInput :: App String
readInput = do
  isEmpty <- S.gets ((== "") . buffer)
  input <- lift $ do
    putStr $ if isEmpty 
      then "\n λ: " 
      else "  | "
    hFlush stdout
    getLine
  modify $ \s -> s { buffer = buffer s ++ input }
  S.gets buffer


display :: App ()
display = do
  state <- S.gets runtimeEnv
  input <- S.gets buffer 
  value <- do  
    let parsed = parser "io" input 
        result = parsed >>= \s -> runRuntime state (runtime s)
    case result of
      Left error -> return error
      Right (ast, globals) -> do
        modify $ \s -> s { runtimeEnv = globals }
        return $ replace "\n" "\n    " (show ast)
  modify $ \s -> s { buffer = "" }
  lift $ do
    putStrLn $ " <| " ++ value
    hFlush stdout


complete xs = impl xs 0
  where impl []     count = count <= 0 
        impl (x:xs) count = case x of
          '(' -> impl xs (count + 1)
          ')' -> impl xs (count - 1)
          _   -> impl xs  count


runApp :: App a -> IO a
runApp app = S.evalStateT app $ AppState "" Map.empty 


{--------------------------- PARSER ---------------------------}


parser :: String -> String -> Either String Statement
parser t s = 
  case Parsec.parse (ignored *> stmt <* (ignored >> eof)) t s of 
    Left error -> Left (show error)
    Right stmt -> Right stmt


stmt = choice . map P.try $ [
    "definition" <??> (SDef  <$> defines)
  , "expression" <??> (SExpr <$> expr)
  ] 


expr = choice . map P.try $ [
    "number"      <??> number
  , "operator"    <??> operator
  , "boolean"     <??> ((XBool . (== 't')) <$> (char '#' *> oneOf "ft"))
  , "nil"         <??> (XNil `when` "nil") 
  , "if"          <??> ifexpr
  , "definition"  <??> (XDef <$> defines)
  , "lambda"      <??> lambdaParser
  , "reference"   <??> (XRef <$> identifier)
  , "application" <??> sexpr
  ]


ifexpr = inParens $ P.string "if" >> ignored >>
  XIf <$> (expr <* ignored)
      <*> (expr <* ignored)
      <*> (expr <* ignored)


sexpr = inParens $
  XApply <$> (ignored *> expr <* ignored)
         <*> (expr `sepEndBy` ignored)


operator = 
  XOperator <$>
    (Add `when` "+" <|> 
     Sub `when` "-" <|> 
     Div `when` "/" <|> 
     Mul `when` "*") 


number = (XNum . read) <$> sign <++> digits <++> decimal 
  where digits  = P.many1 digit
        sign    = P.option "" (P.string "-")
        decimal = P.option "" (char '.' <:> digits)


defines = inParens $ P.string "define" >> ignored >> (function <|> simple)
  where
    simple = Def <$> (identifier <* ignored) <*> expr

    function = do
      (name, args) <- inParens (identifier `sepEndBy` ignored) >>= \case
        name:args -> return (name, args)
        [       ] -> fail "cannot redefine unit"
      body <- functionBody
      return $ Def name (lambda args body)


lambdaParser = inParens $ P.string "lambda" >> do 
  args <- inParens (identifier `sepEndBy` ignored) <* ignored
  body <- functionBody
  return $ lambda args body
  

functionBody = expr `sepEndBy` ignored >>= \case
  [          ] -> return XNil 
  [expression] -> return expression
  expressions  -> return $ XSeq expressions


ignored = comment <|> spaces
  where 
    spaces = void (many $ oneOf " \t\n\f\r")
    comment = do
      spaces 
      many $ do
        P.string ";;" 
        manyTill anyChar newline
        spaces
      spaces


when :: a -> String -> Parser a
when value text = P.string text >> return value


identifier :: Parser String 
identifier = (letter <|> symbol) <:> many (letter <|> digit <|> symbol)
  where symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


inParens :: Parser a -> Parser a 
inParens parser = (char '(' >> spaces) *> parser <* (spaces >> char ')')


(<++>) a b = (++) <$> a <*> b
(<:>) a b  = (:)  <$> a <*> b
(<??>) a b = b <?> a


{--------------------------- RUNTIME ---------------------------}


runRuntime :: Global -> Runtime Expr -> Either String (Expr, Global)
runRuntime state runtime = 
  case I.runIdentity $ E.runErrorT (S.runStateT runtime state) of 
    Right result -> Right result
    Left error   -> Left (show error)
  


type Runtime a = StateT Global (ErrorT LispError Identity) a 
type Global = Map Ref Expr


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


{--------------------------- EVALUATION ---------------------------}


type Eval a = StateT Env (ErrorT LispError Identity) a 
type Scope = [Map Ref Expr]

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


