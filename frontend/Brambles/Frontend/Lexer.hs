{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Brambles.Frontend.Lexer where

import Data.Functor (($>))

import Data.Void

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

reservedWords = ["int", "float", "bool", "fn", "let", "while", "return", "if", "then", "else"]

keyword :: String -> Parser String
keyword kw = lexeme $ C.string kw <* notFollowedBy (C.alphaNumChar <|> C.char '_')

parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"

returnArrow = symbol "->"
assignment  = symbol "="
equality    = symbol "=="

integerType = keyword "int"
floatType   = keyword "float"
boolType    = keyword "bool"

fn          = keyword "fn"
decl        = keyword "let"
while       = keyword "while"
ret         = keyword "return"
ifLex       = keyword "if"
thenLex     = keyword "then"
elseLex     = keyword "else"

bool    = keyword "true" $> True <|> keyword "false" $> False
natural = lexeme L.decimal
float   = lexeme L.float

identifier = do
  ident <- lexeme $ try ((:) <$> C.letterChar <*> many (C.alphaNumChar <|> C.char '_'))
  if ident `elem` reservedWords
    then fail $ "Identifier cannot be a reserved word: " ++ show ident
    else pure ident

