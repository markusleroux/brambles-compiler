module Parser where

import AST (
    BinOp (..),
    Block (..),
    Expr (..),
    Func (..),
    Name,
    Program (..),
    Stmt (..),
    Type (..),
    UnOp (..),
 )
import Data.Either (
    lefts,
    rights,
 )
import Lexer (
    assignment,
    braces,
    colon,
    commas,
    decl,
    float,
    floatType,
    fn,
    identifier,
    integerType,
    lexer,
    natural,
    parens,
    returnArrow,
    semicolon,
    spaceConsumer,
 )
import Text.Parsec (
    eof,
    many,
    try,
    (<?>),
    (<|>),
 )
import qualified Text.Parsec.Expr as Expr
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

typeP :: Parser Type
typeP = TInt <$ integerType <|> TFloat <$ floatType <?> "type"

exprP :: Parser (Expr Name)
exprP = Expr.buildExpressionParser table factorP <?> "expression"
  where
    factorP =
        try (FloatLit <$> float)
            <|> try (IntLit <$> natural)
            <|> try (Call <$> identifier <*> parens (commas exprP))
            <|> try (Assign <$> (identifier <* assignment) <*> exprP)
            <|> try (Var <$> identifier)
            <|> try (EBlock <$> blockP)
            <|> parens exprP

    table =
        [ [unaryOp "-" Neg, unaryOp "+" Pos]
        , [binaryOp "*" Mult Expr.AssocLeft, binaryOp "/" Div Expr.AssocLeft]
        , [binaryOp "+" Add Expr.AssocLeft, binaryOp "-" Sub Expr.AssocLeft]
        ]
      where
        unaryOp name op = Expr.Prefix $ UnOp op <$ Tok.reservedOp lexer name
        binaryOp name op = Expr.Infix $ BinOp op <$ Tok.reservedOp lexer name

-- statement is an expr or a declaration followed by a semi-colon
statementP :: Parser (Stmt Name)
statementP = (assignP <|> (Expr <$> exprP)) <* semicolon
  where
    assignP =
        Decl <$> (decl *> identifier) <*> (colon *> typeP) <*> (assignment *> exprP)

-- a block is many statements wrapped in braces
blockP :: Parser (Block Name)
blockP = braces $ Block <$> many statementP

-- fn name(t0 arg0, ...) -> returnType { ... }
functionP :: Parser (Func Name)
functionP = do
    name <- fn *> identifier
    (vars, params) <- unzip <$> parens (commas varAndTypeP)
    returns <- returnArrow *> typeP
    Func name vars (TCallable params returns) <$> blockP
  where
    varAndTypeP = (,) <$> (identifier <* colon) <*> typeP

-- a program is a list of globals and functions
programP :: Parser (Program Name)
programP = do
    spaceConsumer
    globalsAndFunctions <-
        many $
            eitherParseComb statementP (functionP <* semicolon)
    eof
    return $ Program (lefts globalsAndFunctions) (rights globalsAndFunctions)
  where
    eitherParseComb :: Parser a -> Parser b -> Parser (Either a b)
    eitherParseComb l r = (Left <$> l) <|> (Right <$> r)
