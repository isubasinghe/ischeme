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
  | Nil
  | Bool Bool
  deriving (Show)


