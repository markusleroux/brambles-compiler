module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Expr

import Data.Either

import Lexer
import AST

table = [ [ binaryOp "*" Mult Expr.AssocLeft, binaryOp "/" Div Expr.AssocLeft ]
        , [ binaryOp "+" Add Expr.AssocLeft, binaryOp "-" Sub Expr.AssocLeft ]
        , [ Expr.Infix (Assignment <$ Tok.reservedOp lexer "=") Expr.AssocLeft ]
        ]
    where
        binaryOp name op = Expr.Infix $ BinOp op <$ Tok.reservedOp lexer name

variableP :: Parser Expr
variableP = Var <$> Tok.identifier lexer

callP :: Parser Expr
callP = do
   name <- Tok.identifier lexer
   args <- Tok.parens lexer $ Tok.commaSep lexer exprP
   return $ Call name args

exprP :: Parser Expr
exprP = Expr.buildExpressionParser table factorP
   where
       factorP = try floatP <|> try intP <|> try callP <|> try variableP
       intP = IntLit <$> Tok.integer lexer
       floatP = FloatLit <$> Tok.float lexer

typeP :: Parser Type
typeP = litIntP <|> litFloatP
    where
        litIntP = TyInt <$ Tok.reserved lexer "int"
        litFloatP = TyFloat <$ Tok.reserved lexer "float"

bindP :: Parser Bind
bindP = do
    _type <- typeP
    name <- lookAhead variableP
    return $ Bind { bindType = _type , bindName = name }

functionP :: Parser Function
functionP = do
    Tok.reserved lexer "fn"
    name <- Tok.identifier lexer
    args <- Tok.parens lexer $ Tok.commaSep lexer (bindP <* variableP)  -- remove lookahead
    Tok.reservedOp lexer "->"
    returnType <- typeP

    {- -- fails if bindP fails
    locals <- lookAhead $ Tok.braces lexer $ many $ bindP <* manyTill anyToken ( Tok.reservedOp lexer ";" )
    body <- Tok.braces lexer $ (optional typeP *> exprP) `endBy` (Tok.reservedOp lexer ";") -}

    -- will allow illegal int func(a);
    rawBody <- Tok.braces lexer $ parseEither bindP (optional typeP *> exprP) 
        `endBy` (optional $ Tok.reservedOp lexer ";")
        
    return $ Function 
        { functionName = name
        , functionReturnType = returnType
        , functionArguments = args
        , functionLocals = lefts rawBody
        , functionBody = rights rawBody
        }

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither a b = (Left <$> a) <|> (Right <$> b)
