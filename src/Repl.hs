{-# LANGUAGE OverloadedStrings #-}

module Repl where
import System.IO
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Eval
import AST
import Control.Monad (liftM)

flushStr :: T.Text -> IO ()
flushStr s = TIO.putStr s >> hFlush stdout

readPrompt :: T.Text -> IO T.Text 
readPrompt p = flushStr p >> TIO.getLine

evalAndPrint :: Env -> T.Text -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> T.Text -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr "(lisp) >> " expr) >>= eval env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
    then return ()
    else action result >> until_ pred prompt action

runOne :: T.Text -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "(lisp) >> ") . evalAndPrint