{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Asterius.Types
import           Control.Monad
import           Data.Aeson                 hiding (Object)
import qualified Data.Aeson                 as A
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text                  as T
import Eval
import Repl

main :: IO ()
main = putStrLn "CFW Cabal"

f x = nullEnv >>= (\env -> evalString env x)

parseJSToSchemeO :: JSString -> IO JSString
parseJSToSchemeO s' = toJSString <$> f (T.pack $ fromJSString s')

foreign export javascript "parseJSToSchemeO" parseJSToSchemeO :: JSString -> IO JSString
