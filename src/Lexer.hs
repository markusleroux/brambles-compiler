{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Lexer where

import Data.Functor (($>))
import Text.Parsec (alphaNum, char, letter, oneOf, (<|>))
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser m
lexer = Tok.makeTokenParser languageDef
  where
    opParser = oneOf ":!#$%&*+./<=>?@\\^|-~"
    languageDef =
        Tok.LanguageDef
            { Tok.commentStart = "/*"
            , Tok.commentEnd = "*/"
            , Tok.commentLine = "//"
            , Tok.nestedComments = False
            , -- Identifiers
              Tok.identStart = letter <|> char '_'
            , Tok.identLetter = alphaNum <|> char '_'
            , -- Ops
              Tok.opStart = opParser
            , Tok.opLetter = opParser
            , -- Reserved
              Tok.reservedNames = ["int", "float", "bool", "true", "false", "fn", "let", "while", "return", "if", "then", "else"]
            , Tok.reservedOpNames = ["+", "-", "*", "/", ";", "->", "=", ":", "=="]
            , -- Other
              Tok.caseSensitive = True
            }

spaceConsumer = Tok.whiteSpace lexer

integerType = Tok.reserved lexer "int"

floatType = Tok.reserved lexer "float"

boolType = Tok.reserved lexer "bool"

identifier = Tok.identifier lexer

natural = Tok.integer lexer -- literals are always parsed as positive, c.f. UnOp

float = Tok.float lexer

bool = (Tok.reserved lexer "true" $> True) <|> (Tok.reserved lexer "false" $> False)

parens = Tok.parens lexer

braces = Tok.braces lexer

commas = Tok.commaSep lexer

semicolon = Tok.reservedOp lexer ";"

colon = Tok.colon lexer

fn = Tok.reserved lexer "fn"

returnArrow = Tok.reservedOp lexer "->"

assignment = Tok.reservedOp lexer "="

equality = Tok.reservedOp lexer "=="

decl = Tok.reserved lexer "let"

while = Tok.reserved lexer "while"

ret = Tok.reserved lexer "return"

ifLex = Tok.reserved lexer "if"

thenLex = Tok.reserved lexer "then"

elseLex = Tok.reserved lexer "else"
