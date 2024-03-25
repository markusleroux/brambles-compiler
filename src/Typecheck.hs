{-# LANGUAGE UndecidableInstances #-}
module Typecheck where

import AST
import Parser (SourceLoc)

import Control.Monad.Except (
    MonadError,
 )

data TypeError
    = TypeError
    deriving (Eq)

instance Show TypeError where
    show TypeError = undefined


type instance XEIntLit 'Typed = (SourceLoc, Type)
type instance XEFloatLit 'Typed = (SourceLoc, Type)
type instance XEBoolLit 'Typed = (SourceLoc, Type)
type instance XEVar 'Typed = (SourceLoc, Type)
type instance XEUnOp 'Typed = (SourceLoc, Type)
type instance XEBinOp 'Typed = (SourceLoc, Type)
type instance XECall 'Typed = (SourceLoc, Type)
type instance XEAssign 'Typed = (SourceLoc, Type)
type instance XEBlock 'Typed = (SourceLoc, Type)
type instance XEIf 'Typed = (SourceLoc, Type)

type instance XSExpr 'Typed = (SourceLoc, Type)
type instance XSDecl 'Typed = (SourceLoc, Type)
type instance XSWhile 'Typed = (SourceLoc, Type)
type instance XSReturn 'Typed = (SourceLoc, Type)
type instance XSFunc 'Typed = (SourceLoc, Type)

type instance XBlock 'Typed = (SourceLoc, Type)

type instance XProg 'Typed = (SourceLoc, Type)


inferExpr :: MonadError TypeError m => Expr n 'Parsed -> m (Expr n 'Typed)
inferExpr = undefined

inferStmt :: MonadError TypeError m => Stmt n 'Parsed -> m (Stmt n 'Typed)
inferStmt = undefined

inferBlock :: MonadError TypeError m => Block n 'Parsed -> m (Block n 'Typed)
inferBlock = undefined

inferProg :: MonadError TypeError m => Prog n 'Parsed -> m (Prog n 'Typed)
inferProg = undefined


-- Approach Two: Generate constraint problem and elaboration, solve constraints and fill in elaboration a la Haskell (https://www.youtube.com/watch?v=-TJGhGa04F8)

