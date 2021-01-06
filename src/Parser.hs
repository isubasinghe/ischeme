{-# LANGUAGE OverloadedStrings #-}

module Parser where
import AST as A

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (alphaNumChar, letterChar, char, space, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text hiding (concat)
import Text.Megaparsec.Debug
import Data.Void

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "--")       -- (3)
  (L.skipBlockComment "{-" "-}") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = do
  val <- char '\"' *> manyTill L.charLiteral (char '\"')
  return (pack val)

identifier :: Parser Text
identifier = takeWhile1P (Just "Identifier") (\s -> s /= '(' && s /= ')' && s /= ' ' && s  /= '\n' && s /= '\t')

parseAtom :: Parser A.LispVal
parseAtom = A.Atom <$> identifier <?> "<identifier>"

parseText :: Parser A.LispVal 
parseText = A.String <$> stringLiteral

parseNumber :: Parser A.LispVal
parseNumber = A.Number <$> L.decimal

parseList :: Parser LispVal
parseList = do
  A.List . Prelude.concat <$> many parseExpr `sepBy` (char ' ' <|> char '\n')

parseSExpr :: Parser A.LispVal
parseSExpr = List . concat <$> withParens (many parseExpr `sepBy` (char ' ' <|> char '\n'))

withParens :: Parser a -> Parser a
withParens x = char '(' *> x <* char ')'

parseQuote :: Parser A.LispVal
parseQuote = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseReserved :: Parser A.LispVal
parseReserved = do
  string "Nil" >> return Nil
  <|> (string "#t" >> return (A.Bool True))
  <|> (string "#f" >> return (A.Bool False))

parseExpr:: Parser A.LispVal
parseExpr = choice
  [ parseReserved
  , parseNumber
  , parseQuote
  , parseText
  , parseAtom
  , parseSExpr    
  ]

contents :: Parser a -> Parser a
contents p = do
  r <- p
  eof
  return r