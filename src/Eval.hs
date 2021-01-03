{-# LANGUAGE OverloadedStrings #-}
module Eval where
import qualified Data.Text as T
import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader
import AST
import Control.Exception
import Data.Typeable