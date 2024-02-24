module Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser m
lexer = Tok.makeTokenParser languageDef
    where
        opParser = oneOf ":!#$%&*+./<=>?@\\^|-~"
        languageDef = Tok.LanguageDef
            { Tok.commentStart = "/*"
            , Tok.commentEnd = "*/"
            , Tok.commentLine = "//"
            , Tok.nestedComments = False
            -- Identifiers
            , Tok.identStart = letter <|> char '_'
            , Tok.identLetter = alphaNum <|> char '_'
            -- Ops
            , Tok.opStart = opParser
            , Tok.opLetter = opParser
            -- Reserved
            , Tok.reservedNames = ["fn", "int", "float"]
            , Tok.reservedOpNames = ["+", "-", "*", "/", ";", "->", "="]
            -- Other
            , Tok.caseSensitive = True
            }
