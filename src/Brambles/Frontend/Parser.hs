module Brambles.Frontend.Parser where

import Brambles.Frontend.AST
import Brambles.Frontend.Lexer (
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
    thenLex,
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

import Text.Parsec.String (Parser)
import Text.Parsec (
    eof,
    many,
    optionMaybe,
    try,
    (<?>),
    (<|>),
 )
import qualified Text.Parsec.Expr as Expr
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
type instance XEIf 'Parsed = SourceLoc
type instance XEFunc 'Parsed = (SourceLoc, Type)

type instance XSExpr 'Parsed = SourceLoc
type instance XSDecl 'Parsed = (SourceLoc, Type)
type instance XSWhile 'Parsed = SourceLoc
type instance XSReturn 'Parsed = SourceLoc

type instance XBlock 'Parsed = SourceLoc
type instance XProg 'Parsed = SourceLoc


typeP :: Parser Type
typeP = integerP <|> floatP <|> boolP <|> callableP <?> "type"
  where
    integerP  = TInt      <$  integerType
    floatP    = TFloat    <$  floatType
    boolP     = TBool     <$  boolType
    callableP = TCallable <$> parens (commas typeP) <*> (returnArrow *> typeP)

varP :: Parser (Var Name)
varP = V <$> identifier

blockP :: Parser (Block Name 'Parsed)
blockP = braces $ Block  SourceLoc <$> many (try statementP) <*> optionMaybe exprP

exprP :: Parser (Expr Name 'Parsed)
exprP = 
  let
    factorP 
        = try floatLitP
      <|> try intLitP
      <|> try boolLitP
      <|> try callP
      <|> try assignP
      <|> try evarP
      <|> eblockP
      <|> functionP
      <|> ifP
      <|> parens exprP
  in
    Expr.buildExpressionParser table factorP <?> "expression"
  where
    floatLitP = EFloatLit SourceLoc <$> float
    intLitP   = EIntLit   SourceLoc <$> natural
    boolLitP  = EBoolLit  SourceLoc <$> bool

    callP   = ECall   SourceLoc <$> (evarP <|> parens exprP) <*> parens (commas exprP)
    assignP = EAssign SourceLoc <$> (varP <* assignment) <*> exprP
    evarP   = EVar    SourceLoc <$> varP
    eblockP = EBlock            <$> blockP
    ifP     = EIf     SourceLoc <$> (ifLex *> exprP) <*> (thenLex *> blockP) <*> optionMaybe (elseLex *> blockP)

    table =
        [ [unaryOp "-" Neg, unaryOp "+" Pos]
        , [binaryOp "*" Mult Expr.AssocLeft, binaryOp "/" Div Expr.AssocLeft]
        , [binaryOp "+" Add Expr.AssocLeft, binaryOp "-" Sub Expr.AssocLeft]
        , [binaryOp "==" Eq Expr.AssocLeft]
        ]
      where
        unaryOp  name op = Expr.Prefix $ EUnOp SourceLoc op <$ Tok.reservedOp lexer name
        binaryOp name op = Expr.Infix $ EBinOp SourceLoc op <$ Tok.reservedOp lexer name

    -- fn name(t0 arg0, ...) -> returnType { ... }
    functionP = do
        name <- fn *> varP
        (vars, params) <- unzip <$> parens (commas varAndTypeP)
        returns <- returnArrow *> typeP
        EFunc (SourceLoc, TCallable params returns) name vars <$> blockP
      where
        varAndTypeP = (,) <$> (varP <* colon) <*> typeP

statementP :: Parser (Stmt Name 'Parsed)
statementP = 
  let
    stmtP = declP <|> sExprP <|> whileP <|> returnP <?> "statement"
  in
    stmtP <* semicolon
  where
    sExprP = SExpr SourceLoc <$> exprP
    whileP = SWhile SourceLoc <$> (while *> exprP) <*> bracedStmtsP
    declP = do
      var <- decl *> varP
      type_ <- colon *> typeP
      SDecl (SourceLoc, type_) var <$> (assignment *> exprP)
    returnP = SReturn SourceLoc <$> (ret *> exprP)


bracedStmtsP :: Parser [Stmt Name 'Parsed]
bracedStmtsP = braces $ many statementP

programP :: Parser (Prog Name 'Parsed)
programP = let stmtsP = spaceConsumer *> many statementP <* eof
  in Globals SourceLoc <$> stmtsP

