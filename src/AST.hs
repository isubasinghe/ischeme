{-# LANGUAGE OverloadedStrings #-}

module AST where
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Megaparsec ( ParseErrorBundle )
import Data.Void (Void)

import Control.Monad.Except ( ExceptT )
import Data.IORef ( IORef )
import Data.List (intercalate)

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show


type IOThrowsError = ExceptT LispError IO
type ThrowsError = Either LispError
type Env = IORef (M.Map T.Text (IORef LispVal))

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Nil
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  |  Func { params :: [T.Text], vararg :: Maybe T.Text,
            body :: [LispVal], closure :: Env }

debugLispVal :: LispVal -> String
debugLispVal (Atom n) = "Atom " ++ T.unpack n
debugLispVal (List ls) = "List [" ++ intercalate "," (map debugLispVal ls) ++ "]"
debugLispVal (Number n) = "Atom " ++ show n
debugLispVal (String t) = "String " ++ T.unpack t
debugLispVal Nil = "Nil"
debugLispVal (Bool t) = "Bool " ++ show t
debugLispVal (PrimitiveFunc pfunc) = "PrimitiveFunc ([LispVal] -> ThrowsError LispVal)"
debugLispVal Func {params = args, vararg = varargs, body = body, closure = env} = 
  "(Func (" ++ unwords (map show args) ++
    (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ T.unpack arg) ++ ") ...)"

showLispVal :: LispVal -> String
showLispVal (String contents) = "\"" ++ T.unpack contents ++ "\""
showLispVal (Atom name) = T.unpack name
showLispVal (Number contents) = show contents
showLispVal (Bool True) = "#t"
showLispVal (Bool False) = "#f"
showLispVal (List ls) = "(" ++ unwordsList ls ++ ")"
showLispVal Nil = "(nil)"
showLispVal (PrimitiveFunc _) = "(pfunc)"
showLispVal Func {params = args, vararg = varargs, body = body, closure = env} =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ T.unpack arg) ++ ") ...)"


instance Show LispVal where show = showLispVal


data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch T.Text LispVal
  | Parser (ParseErrorBundle T.Text Void)
  | BadSpecialForm T.Text LispVal
  | NotFunction T.Text T.Text
  | UnboundVar T.Text T.Text
  | Default T.Text

showError :: LispError -> String
showError (UnboundVar message varname)  = T.unpack message ++ ": " ++ T.unpack varname
showError (BadSpecialForm message form) = T.unpack message ++ ": " ++ show form
showError (NotFunction message func)    = T.unpack message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ T.unpack expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError