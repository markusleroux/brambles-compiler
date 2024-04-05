module Brambles.Frontend.Parser where

import Brambles.Frontend.AST
import Brambles.Frontend.Lexer (Parser)
import qualified Brambles.Frontend.Lexer as L

import Text.Megaparsec
import Control.Monad.Combinators.Expr

data SourceLoc = SourceLoc
  deriving (Show, Eq)

type instance XEIntLit   'Parsed = SourceLoc
type instance XEFloatLit 'Parsed = SourceLoc
type instance XEBoolLit  'Parsed = SourceLoc
type instance XEVar      'Parsed = SourceLoc
type instance XEUnOp     'Parsed = SourceLoc
type instance XEBinOp    'Parsed = SourceLoc
type instance XECall     'Parsed = SourceLoc
type instance XEAssign   'Parsed = SourceLoc
type instance XEIf       'Parsed = SourceLoc
type instance XEFunc     'Parsed = (SourceLoc, Type)

type instance XSExpr     'Parsed = SourceLoc
type instance XSDecl     'Parsed = (SourceLoc, Type)
type instance XSWhile    'Parsed = SourceLoc
type instance XSReturn   'Parsed = SourceLoc

type instance XBlock     'Parsed = SourceLoc
type instance XProg      'Parsed = SourceLoc


parseGuardMb :: Parser a -> Parser b -> Parser (Maybe b)
parseGuardMb kw p = do
  o <- getOffset
  observing kw >>= \case
    Left _ -> setOffset o >> pure Nothing
    Right _ -> pure <$> p

parseGuard :: Parser a -> Parser b -> Parser b
parseGuard kw p = do
  o <- getOffset
  observing kw >>= \case
    Left err -> setOffset o >> parseError err
    Right _ -> p

typeP :: Parser Type
typeP = integerP <|> floatP <|> boolP <|> callableP <?> "type"
  where
    integerP  = TInt      <$  L.integerType <?> "int type"
    floatP    = TFloat    <$  L.floatType <?> "float type"
    boolP     = TBool     <$  L.boolType <?> "bool type"
    callableP = TCallable <$> L.parens (typeP `sepBy` L.comma) <*> (L.returnArrow *> typeP) <?> "callable type"

varP :: Parser (Var Name)
varP = V <$> L.identifier <?> "variable"

blockP :: Parser (Block Name 'Parsed)
blockP = L.braces $ Block  SourceLoc <$> many (try statementP) <*> optional exprP

exprP :: Parser (Expr Name 'Parsed)
exprP = 
  let  -- TODO: use less backtracking
    factorP =
          try floatLitP
      <|> intLitP
      <|> boolLitP
      <|> eblockP
      <|> functionP
      <|> ifP
      <|> try callP
      <|> try assignP
      <|> try evarP
      <|> L.parens exprP
  in
    makeExprParser factorP table <?> "expression"
  where
    floatLitP = EFloatLit SourceLoc <$> L.float   <?> "float"
    intLitP   = EIntLit   SourceLoc <$> L.natural <?> "int"
    boolLitP  = EBoolLit  SourceLoc <$> L.bool    <?> "boolean"

    callP   = ECall   SourceLoc <$> evarP <*> L.parens (exprP `sepBy` L.comma) <?> "call"
    assignP = EAssign SourceLoc <$> (varP <* L.assignment) <*> exprP <?> "assignment"
    evarP   = EVar    SourceLoc <$> varP   <?> "variable"
    eblockP = EBlock            <$> blockP <?> "block"
    ifP     = parseGuard L.ifLex (EIf SourceLoc <$> exprP <*> (L.thenLex *> blockP) <*> parseGuardMb L.elseLex blockP <?> "block")

    table =
        [ [prefix "-" Neg,  prefix "+" Pos]
        , [binary "*" Mult, binary "/" Div]
        , [binary "+" Add,  binary "-" Sub]
        , [binary "==" Eq]
        ]
      where
        prefix name f = Prefix (EUnOp SourceLoc f <$ L.symbol name)
        binary name f = InfixL (EBinOp SourceLoc f <$ L.symbol name)

    -- fn name(t0 arg0, ...) -> returnType { ... }
    functionP = parseGuard L.fn $ do
        name <- varP
        (vars, params) <- unzip <$> L.parens (varAndTypeP `sepBy` L.comma)
        returns <- L.returnArrow *> typeP
        EFunc (SourceLoc, TCallable params returns) name vars <$> blockP
      where
        varAndTypeP = (,) <$> (varP <* L.colon) <*> typeP

statementP :: Parser (Stmt Name 'Parsed)
statementP = 
  let
    stmtP = declP <|> whileP <|> returnP <|> sExprP <?> "statement"
  in
    stmtP <* L.semicolon
  where
    sExprP = SExpr SourceLoc <$> exprP
    whileP = parseGuard L.while $ SWhile SourceLoc <$> exprP <*> bracedStmtsP
    declP = parseGuard L.decl $ do
      var   <- varP
      type_ <- L.colon *> typeP
      SDecl (SourceLoc, type_) var <$> (L.assignment *> exprP)
    returnP = parseGuard L.ret $ SReturn SourceLoc <$> exprP


bracedStmtsP :: Parser [Stmt Name 'Parsed]
bracedStmtsP = L.braces $ many statementP

programP :: Parser (Prog Name 'Parsed)
programP = let stmtsP = L.spaceConsumer *> many statementP <* eof
  in Globals SourceLoc <$> stmtsP

