{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import qualified Control.Monad.Identity as I
import qualified Control.Monad.State as S
import qualified Data.Map as Map

import Control.Monad.State (State, StateT, get, modify, lift, void)
import Control.Monad (liftM)
import Data.List.Utils (replace)

import Debug.Trace (trace)

import System.IO (hFlush, stdout)

import Runtime
import Parser
import Eval


type App a = StateT AppState IO a

data AppState = AppState { buffer :: String, runtimeEnv :: Global }


main = runApp app 


app = readInput >>= \case
  [            ] -> lift $ putStrLn " Ciao\n"
  i | complete i -> display >> app
  notFinishedYet -> app


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


