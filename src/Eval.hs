{-# LANGUAGE OverloadedStrings #-}
module Eval where
import qualified Data.Text as T
import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader
import AST
import Control.Exception
import Data.Typeable

data LispError 
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
  deriving (Show)

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads (T.unpack n) :: [(Integer, String)] in 
                            if null parsed 
                              then 0
                              else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params


primitives :: M.Map T.Text ([LispVal] -> LispVal)
primitives = M.fromList 
              [ ("+", numericBinop (+))
              , ("-", numericBinop (-))
              , ("*", numericBinop (*))
              , ("/", numericBinop div)
              , ("mod", numericBinop mod)
              , ("quotient", numericBinop quot)
              , ("remainder", numericBinop rem)
              ]

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val

eval (List (Atom func : args)) = apply func $ map eval args


apply :: T.Text -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ M.lookup func primitives
