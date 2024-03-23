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

typeP :: Parser Type
typeP =
    TInt <$ integerType
        <|> TFloat <$ floatType
        <|> TBool <$ boolType
        <|> (TCallable <$> parens (commas typeP) <*> (returnArrow *> typeP))
        <?> "type"

varP :: Parser (Var Name)
varP = V <$> identifier

exprP :: Parser (Expr Name)
exprP = Expr.buildExpressionParser table factorP <?> "expression"
  where
    factorP =
        try (EFloatLit <$> float)
            <|> try (EIntLit <$> natural)
            <|> try (EBoolLit <$> bool)
            <|> try (ECall <$> varP <*> parens (commas exprP))
            <|> try (EAssign <$> (varP <* assignment) <*> exprP)
            <|> try (EVar <$> varP)
            <|> try (EBlock <$> blockP)
            <|> try ifP
            <|> parens exprP

    table =
        [ [unaryOp "-" Neg, unaryOp "+" Pos]
        , [binaryOp "*" Mult Expr.AssocLeft, binaryOp "/" Div Expr.AssocLeft]
        , [binaryOp "+" Add Expr.AssocLeft, binaryOp "-" Sub Expr.AssocLeft]
        , [binaryOp "==" Eq Expr.AssocLeft]
        ]
      where
        unaryOp name op = Expr.Prefix $ EUnOp op <$ Tok.reservedOp lexer name
        binaryOp name op = Expr.Infix $ EBinOp op <$ Tok.reservedOp lexer name

    ifP = EIf <$> (ifLex *> parens exprP) <*> blockP <*> optionMaybe (elseLex *> blockP)

-- statement is an expr or a declaration followed by a semi-colon
statementP :: Parser (Stmt Name)
statementP = stmtP <* semicolon
  where
    stmtP =
        declP
            <|> (SExpr <$> exprP)
            <|> whileP
            <|> returnP
            <|> functionP
            <?> "statement"

    declP = SDecl <$> (decl *> varP) <*> (colon *> typeP) <*> (assignment *> exprP)
    whileP = SWhile <$> (while *> exprP) <*> blockP
    returnP = SReturn <$> (ret *> exprP)

    -- fn name(t0 arg0, ...) -> returnType { ... }
    functionP :: Parser (Stmt Name)
    functionP = do
        name <- fn *> varP
        (vars, params) <- unzip <$> parens (commas varAndTypeP)
        returns <- returnArrow *> typeP
        SFunc name vars (TCallable params returns) <$> blockP
      where
        varAndTypeP = (,) <$> (varP <* colon) <*> typeP

blockP :: Parser (Block Name)
blockP = braces $ Block <$> many statementP

programP :: Parser (Prog Name)
programP = Globals <$> (spaceConsumer *> many statementP <* eof)
