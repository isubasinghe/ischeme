{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module AST where
import qualified Data.Text as T
import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader



data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  deriving (Show)

type EnvCtx = M.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
  deriving ( Monad
          , Functor
          , Applicative
          , MonadReader EnvCtx
          , MonadIO)

instance Show (Eval a) where 
  show x = "(eval)"

newtype IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

instance Show IFunc where
  show _ = "(internal function)"


