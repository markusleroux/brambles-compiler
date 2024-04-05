{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Brambles.Frontend.Lexer where

import Protolude hiding (try)
import Control.Monad.Fail (fail)

import Text.Megaparsec (Parsec, between, notFollowedBy, try, tokensToChunk)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

reservedWords = ["int", "float", "bool", "fn", "let", "while", "return", "if", "then", "else"]

keyword :: Text -> Parser Text
keyword kw = lexeme $ C.string kw <* notFollowedBy (C.alphaNumChar <|> C.char '_')

parens :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces    = between (symbol "{") (symbol "}")

semicolon :: Parser Text
semicolon = symbol ";"

comma :: Parser Text
comma     = symbol ","

colon :: Parser Text
colon     = symbol ":"

returnArrow :: Parser Text
returnArrow = symbol "->"

assignment :: Parser Text
assignment  = symbol "="

equality :: Parser Text
equality    = symbol "=="

integerType :: Parser Text
integerType = keyword "int"

floatType :: Parser Text
floatType   = keyword "float"

boolType :: Parser Text
boolType    = keyword "bool"

fn :: Parser Text
fn          = keyword "fn"

decl :: Parser Text
decl        = keyword "let"

while :: Parser Text
while       = keyword "while"

ret :: Parser Text
ret         = keyword "return"

ifLex :: Parser Text
ifLex       = keyword "if"

thenLex :: Parser Text
thenLex     = keyword "then"

elseLex :: Parser Text
elseLex     = keyword "else"

bool :: Parser Bool
bool    = keyword "true" $> True <|> keyword "false" $> False

natural :: Parser Integer
natural = lexeme L.decimal

float :: Parser Double
float   = lexeme L.float

identifier :: Parser Text
identifier = do
  ident <- lexeme $ try ((:) <$> C.letterChar <*> many (C.alphaNumChar <|> C.char '_'))
  if ident `elem` reservedWords
    then fail $ "Identifier cannot be a reserved word: " ++ show ident
    else pure $ tokensToChunk (Proxy :: Proxy Text) ident

