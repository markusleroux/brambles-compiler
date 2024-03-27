{-# LANGUAGE StandaloneDeriving #-}
module Parser where

import AST
import Lexer (
    assignment,
    bool,
    boolType,
    braces,
    colon,
    commas,
    decl,
    elseLex,
    float,
    floatType,
    fn,
    identifier,
    ifLex,
    integerType,
    lexer,
    natural,
    parens,
    ret,
    returnArrow,
    semicolon,
    spaceConsumer,
    while,
 )
import Text.Parsec (
    eof,
    many,
    optionMaybe,
    try,
    (<?>),
    (<|>),
 )
import qualified Text.Parsec.Expr as Expr
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

data SourceLoc = SourceLoc
  deriving (Show, Eq)

type instance XEIntLit 'Parsed = SourceLoc
type instance XEFloatLit 'Parsed = SourceLoc
type instance XEBoolLit 'Parsed = SourceLoc
type instance XEVar 'Parsed = SourceLoc
type instance XEUnOp 'Parsed = SourceLoc
type instance XEBinOp 'Parsed = SourceLoc
type instance XECall 'Parsed = SourceLoc
type instance XEAssign 'Parsed = SourceLoc
type instance XEBlock 'Parsed = SourceLoc
type instance XEIf 'Parsed = SourceLoc

type instance XSExpr 'Parsed = SourceLoc
type instance XSDecl 'Parsed = (SourceLoc, Type)
type instance XSWhile 'Parsed = SourceLoc
type instance XSReturn 'Parsed = SourceLoc
type instance XSFunc 'Parsed = (SourceLoc, Type)

type instance XBlock 'Parsed = SourceLoc

type instance XProg 'Parsed = SourceLoc

typeP :: Parser Type
typeP =
    TInt <$ integerType
        <|> TFloat <$ floatType
        <|> TBool <$ boolType
        <|> (TCallable <$> parens (commas typeP) <*> (returnArrow *> typeP))
        <?> "type"

varP :: Parser (Var Name)
varP = V <$> identifier

exprP :: Parser (Expr Name 'Parsed)
exprP = Expr.buildExpressionParser table factorP <?> "expression"
  where
    factorP =
        try (EFloatLit SourceLoc <$> float)
            <|> try (EIntLit SourceLoc <$> natural)
            <|> try (EBoolLit SourceLoc <$> bool)
            <|> try (ECall SourceLoc <$> varP <*> parens (commas exprP))
            <|> try (EAssign SourceLoc <$> (varP <* assignment) <*> exprP)
            <|> try (EVar SourceLoc <$> varP)
            <|> try (EBlock SourceLoc <$> blockP)
            <|> try ifP
            <|> parens exprP

    table =
        [ [unaryOp "-" Neg, unaryOp "+" Pos]
        , [binaryOp "*" Mult Expr.AssocLeft, binaryOp "/" Div Expr.AssocLeft]
        , [binaryOp "+" Add Expr.AssocLeft, binaryOp "-" Sub Expr.AssocLeft]
        , [binaryOp "==" Eq Expr.AssocLeft]
        ]
      where
        unaryOp name op = Expr.Prefix $ EUnOp SourceLoc op <$ Tok.reservedOp lexer name
        binaryOp name op = Expr.Infix $ EBinOp SourceLoc op <$ Tok.reservedOp lexer name

    ifP = EIf SourceLoc <$> (ifLex *> parens exprP) <*> blockP <*> optionMaybe (elseLex *> blockP)

-- statement is an expr or a declaration followed by a semi-colon
statementP :: Parser (Stmt Name 'Parsed)
statementP = stmtP <* semicolon
  where
    stmtP =
        declP
            <|> (SExpr SourceLoc <$> exprP)
            <|> whileP
            <|> returnP
            <|> functionP
            <?> "statement"

    declP = do
      var <- decl *> varP
      type_ <- colon *> typeP
      SDecl (SourceLoc, type_) var <$> (assignment *> exprP)
    whileP = SWhile SourceLoc <$> (while *> exprP) <*> blockP
    returnP = SReturn SourceLoc <$> (ret *> exprP)

    -- fn name(t0 arg0, ...) -> returnType { ... }
    functionP :: Parser (Stmt Name 'Parsed)
    functionP = do
        name <- fn *> varP
        (vars, params) <- unzip <$> parens (commas varAndTypeP)
        returns <- returnArrow *> typeP
        SFunc (SourceLoc, (TCallable params returns)) name vars  <$> blockP
      where
        varAndTypeP = (,) <$> (varP <* colon) <*> typeP

blockP :: Parser (Block Name 'Parsed)
blockP = braces $ Block SourceLoc <$> many statementP

programP :: Parser (Prog Name 'Parsed)
programP = Globals SourceLoc <$> (spaceConsumer *> many statementP <* eof)
