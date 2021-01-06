{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Eval where
import qualified Data.Text as T
import qualified Data.Map as M

import Control.Monad.Except
    ( MonadIO(liftIO), runExceptT, MonadError(..) )
import Control.Monad.Reader
import AST
import Parser ( parseExpr, parseSExprs )
import Text.Megaparsec ( parse, errorBundlePretty )
import Data.Functor ((<&>))
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.Maybe(isNothing, isJust)

nullEnv :: IO Env
nullEnv = newIORef M.empty

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

isBound :: Env -> T.Text -> IO Bool
isBound envRef var = readIORef envRef <&> isJust . M.lookup var

getVar :: Env -> T.Text -> IOThrowsError LispVal
getVar envRef var  =  do 
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (M.lookup var env)

setVar :: Env -> T.Text -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do 
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . flip writeIORef value)
        (M.lookup var env)
  return value

defineVar :: Env -> T.Text -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
      then setVar envRef var value >> return value
      else liftIO $ do
        valueRef <- newIORef value
        env <- readIORef envRef
        writeIORef envRef $ M.insert var valueRef env
        return value

bindVars :: Env -> [(T.Text, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = fmap (\s -> M.fromList s `M.union` env) (mapM addBinding bindings)
          addBinding (var, value) = do 
            ref <- newIORef value
            return (var, ref)


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
  unpacked1 <- unpacker arg1
  unpacked2 <- unpacker arg2
  return $ unpacked1 == unpacked2
  `catchError` const  (return False)


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


subtractOrNegate :: [LispVal] -> ThrowsError LispVal
subtractOrNegate [] = throwError $ NumArgs 2 []
subtractOrNegate [x] = unpackNum x <&> (\n -> Number(-1 * n))
subtractOrNegate xs = mapM unpackNum xs <&> (Number . foldl1 (-))

primitives :: M.Map T.Text ([LispVal] -> ThrowsError LispVal)
primitives = M.fromList 
              [ ("+", numericBinop (+))
              , ("-", subtractOrNegate)
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

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map makePrimitiveFunc $ M.toAscList primitives)
    where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

makeFunc :: Monad m => Maybe T.Text -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map (T.pack . showLispVal) params) varargs body env
makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing
makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . T.pack . showLispVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
-- Clearly not functional code here, although the above isn't as well, the order is important
-- and everything must be evaluated this is why last $ map will not work, since Haskell applies rewrite rules
-- and only evaluates the very last LispVal
eval env (List' ls) = eval' env ls Nothing
  where
    eval' :: Env -> [LispVal] -> Maybe LispVal -> IOThrowsError LispVal
    eval' env [] p = maybe (return $ List []) return p
    eval' env (x:xs) p = do 
      o <- eval env x
      eval' env xs (Just o)
      

eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
      if num params /= num args && isNothing varargs
        then throwError $ NumArgs (num params) args
        else liftIO ( bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = last <$> mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env

readExpr :: String -> T.Text -> ThrowsError LispVal
readExpr f s = case parse parseSExprs f s of
                Right val -> return val
                Left err -> throwError $ Parser $ errorBundlePretty err
