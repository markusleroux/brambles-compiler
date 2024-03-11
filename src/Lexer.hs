{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Lexer where

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
              Tok.reservedNames = ["fn", "int", "float", "let"]
            , Tok.reservedOpNames = ["+", "-", "*", "/", ";", "->", "=", ":"]
            , -- Other
              Tok.caseSensitive = True
            }

spaceConsumer = Tok.whiteSpace lexer

integerType = Tok.reserved lexer "int"

floatType = Tok.reserved lexer "float"

identifier = Tok.identifier lexer

natural = Tok.integer lexer -- literals are always parsed as positive, c.f. UnOp

float = Tok.float lexer

parens = Tok.parens lexer

braces = Tok.braces lexer

commas = Tok.commaSep lexer

semicolon = Tok.reservedOp lexer ";"

colon = Tok.colon lexer

fn = Tok.reserved lexer "fn"

returnArrow = Tok.reservedOp lexer "->"

assignment = Tok.reservedOp lexer "="

decl = Tok.reserved lexer "let"
