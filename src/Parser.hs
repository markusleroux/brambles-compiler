module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Expr

import Data.Either

import Lexer
import AST


typeP :: Parser Type
typeP = TInt   <$ integerType
    <|> TFloat <$ floatType
    <?> "type"


variableP :: Parser Variable
variableP = try (TypedVar   <$> typeP <*> identifier)
        <|> try (UntypedVar <$> identifier)
        <?> "variable"


exprP :: Parser Expr
exprP = Expr.buildExpressionParser table factorP <?> "expression"
    where
        factorP = try (FloatLit <$> float)
              <|> try (IntLit   <$> integer) 
              <|> try (Call     <$> identifier <*> (parens $ commas exprP))
              <|> try (Variable <$> variableP)

        table = [ [ binaryOp "*" Mult Expr.AssocLeft, binaryOp "/" Div Expr.AssocLeft ]
                , [ binaryOp "+" Add Expr.AssocLeft,  binaryOp "-" Sub Expr.AssocLeft ]
                , [ Expr.Infix (Assignment <$ assignment) Expr.AssocLeft ]
                ]

        binaryOp name op = Expr.Infix $ BinOp op <$ Tok.reservedOp lexer name


blockP :: Parser Block
blockP = braces $ Block <$> exprP `endBy` semicolon


functionP :: Parser Function  -- fn name(t0 arg0, ...) -> returnType { ... }
functionP = Function
        <$> (fn *> identifier)
        <*> (parens $ commas variableP) 
        <*> (returnArrow *> typeP)
        <*> blockP
        <?> "function"


programP :: Parser Program
programP = do
        spaceConsumer
        globalsAndFunctions <- eitherParseComb exprP functionP `endBy` semicolon
        eof
        return $ Program (lefts globalsAndFunctions) (rights globalsAndFunctions)
    where
        eitherParseComb :: Parser a -> Parser b -> Parser (Either a b)
        eitherParseComb l r = (Left <$> l) <|> (Right <$> r)

