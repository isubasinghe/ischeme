module Main where
import AST
import Parser
import qualified Eval as E
import Text.Megaparsec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Void
import Control.Arrow

main = do
    s <- TIO.getContents
    let res = parse parseExpr "" s
    case res of
        (Right a) -> do
            print a
        (Left e) -> print e
