{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Eval where
import qualified Data.Text as T
import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader
import AST
import Control.Exception
import Data.Typeable
import Parser
import Text.Megaparsec
import Data.Void (Void)
import Data.Functor ((<&>))

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch T.Text LispVal
  | Parser (ParseErrorBundle T.Text Void)
  | BadSpecialForm T.Text LispVal
  | NotFunction T.Text T.Text
  | UnboundVar T.Text T.Text
  | Default T.Text

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` const  (return False)

showError :: LispError -> String
showError (UnboundVar message varname)  = T.unpack message ++ ": " ++ T.unpack varname
showError (BadSpecialForm message form) = T.unpack message ++ ": " ++ show form
showError (NotFunction message func)    = T.unpack message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ T.unpack expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads (T.unpack n) :: [(Integer, String)] in 
                            if null parsed 
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError T.Text
unpackStr (String s) = return s
unpackStr (Number s) = return $ T.pack $ show s
unpackStr (Bool s)   = return $ T.pack $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params <&> (Number . foldl1 op)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                              then throwError $ NumArgs 2 args
                              else do left <- unpacker $ head args
                                      right <- unpacker $ args !! 1
                                      return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop  = boolBinop unpackNum

strBoolBinop :: (T.Text -> T.Text -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

primitives :: M.Map T.Text ([LispVal] -> ThrowsError LispVal)
primitives = M.fromList 
              [ ("+", numericBinop (+))
              , ("-", numericBinop (-))
              , ("*", numericBinop (*))
              , ("/", numericBinop div)
              , ("mod", numericBinop mod)
              , ("quotient", numericBinop quot)
              , ("remainder", numericBinop rem)
              , ("=", numBoolBinop (==))
              , ("<", numBoolBinop (<))
              , (">", numBoolBinop (>))
              , ("/=", numBoolBinop (/=))
              , (">=", numBoolBinop (>=))
              , ("<=", numBoolBinop (<=))
              , ("&&", boolBoolBinop (&&))
              , ("||", boolBoolBinop (||))
              , ("string=?", strBoolBinop (==))
              , ("string<?", strBoolBinop (<))
              , ("string>?", strBoolBinop (>))
              , ("string<=?", strBoolBinop (<=))
              , ("string>=?", strBoolBinop (>=))
              , ("car", car)
              , ("cdr", cdr)
              , ("cons", cons)
              , ("eq?", eqv)
              , ("eqv?", eqv)
              , ("equal?", equal)
              ]

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    _ -> eval conseq

eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) && 
                                                            all eqvPair (zip arg1 arg2)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
                            Left err -> False
                            Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2) 
                        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


apply :: T.Text -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (M.lookup func primitives)

readExpr :: String -> T.Text -> ThrowsError LispVal
readExpr f s = case parse parseExpr f s of
                Right val -> return val
                Left err -> throwError $ Parser err

interpret :: T.Text -> ThrowsError LispVal
interpret s = readExpr "(ischeme)" s >>= eval