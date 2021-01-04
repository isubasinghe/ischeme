{-# LANGUAGE OverloadedStrings #-}

module Repl where
import System.IO
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Eval
import Control.Monad (liftM)

flushStr :: T.Text -> IO ()
flushStr s = TIO.putStr s >> hFlush stdout

readPrompt :: T.Text -> IO T.Text 
readPrompt p = flushStr p >> TIO.getLine

evalString :: T.Text -> IO String
evalString expr = return $ extractValue $ trapError (fmap show $ interpret expr >>= eval)

evalAndPrint :: T.Text -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== ":quit") (readPrompt "Lisp>>> ") evalAndPrint