module Main where
import AST
import Parser
import Eval
import Text.Megaparsec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = do
    s <- TIO.getContents
    let res = parse parseExpr "" s
    case res of
        (Right a) -> do
            print a
        (Left e) -> print e
