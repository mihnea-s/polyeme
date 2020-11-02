module Main where

import Control.Monad.Except
import Data.IORef
import Polyeme
import Prompt
import System.Console.Haskeline
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  env <- newIORef [] >>= errOnExcept . preload

  if length args == 0
    then do
      putWelcomeMsg
      runInputT defaultSettings $ promptLoop env promptIdle
    else do
      filename <- return (args !! 0)
      contents <- readFile filename
      parsed <- errOnExcept $ readExprs contents filename
      errOnExcept $ evalExprs env parsed
  where
    evalExprs :: Environment -> [Datum] -> Result ()
    evalExprs env (x : xs) = eval env x >> evalExprs env xs
    evalExprs ___ [] = return ()

promptLoop :: Environment -> String -> InputT IO ()
promptLoop env prompt = do
  line <- getInputLine prompt
  case line of
    Nothing -> return ()
    Just "" -> promptLoop env promptIdle
    Just line -> do
      parsed <- liftIO $ errOnExcept $ readExpr line "<stdin>"
      result <- liftIO $ runExceptT $ eval env parsed
      outputStrLn $ either show show result
      promptLoop env $ either (const promptErr) (const promptOk) result

errOnExcept :: ExceptT Error IO r -> IO r
errOnExcept = (liftM $ either (error . show) id) . runExceptT
