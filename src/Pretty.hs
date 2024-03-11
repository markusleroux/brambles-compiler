{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty where

import Prelude hiding (exp)

import Prettyprinter (Pretty(pretty), Doc, (<+>), equals, colon, semi, align, tupled, enclose, braces, vsep, line)
import AST (Program(..), Func(..), Stmt(..), Block(..), Expr(..), UnOp(..), BinOp(..), Type(..))

import Data.Bool (bool)

-- TODO: 
    -- proper formatting
    -- command line access

instance Pretty UnOp where
    pretty Neg = pretty "-"
    pretty Pos = pretty "+"

instance Pretty BinOp where
    pretty Add  = pretty "+"
    pretty Sub  = pretty "-"
    pretty Mult = pretty "*"
    pretty Div  = pretty "/"

instance Pretty Type where
    pretty TInt            = pretty "int"
    pretty TFloat          = pretty "float"
    pretty (TCallable _ _) = undefined


instance Pretty n => Pretty (Expr n) where
    pretty (IntLit val)         = pretty val
    pretty (FloatLit val)       = pretty val
    pretty (Var var)            = pretty var
    pretty (UnOp op exp)        = pretty op <> prettyGrouped exp
    pretty (BinOp op expl expr) = prettyGrouped expl <+> pretty op <+> prettyGrouped expr
    pretty (Call name exps)     = pretty name <> align (tupled $ pretty <$> exps)
    pretty (Assign name exp)    = pretty name <+> equals <+> pretty exp
    pretty (EBlock block)       = pretty block
        

prettyGrouped :: Pretty n => Expr n -> Doc ann
prettyGrouped lit@(IntLit _)   = pretty lit
prettyGrouped lit@(FloatLit _) = pretty lit
prettyGrouped var@(Var _)      = pretty var
prettyGrouped call@(Call _ _)  = pretty call
prettyGrouped p                = pretty "(" <> pretty p <> pretty ")"

instance Pretty n => Pretty (Stmt n) where
    pretty (Expr exp) = pretty exp <> semi
    pretty Decl {..} = pretty "let" <+> pretty sName <> colon <+> pretty sType <+> equals <+> pretty sVal <> semi

instance Pretty n => Pretty (Block n) where
    pretty (Block stmts) = braces . newlineIf (null stmts) . vsep $ pretty <$> stmts
        where 
            newlineIf = bool (enclose line line) id

instance Pretty n => Pretty (Func n) where
    pretty Func {..} 
              = pretty "fn"
            <+> pretty fName
             <> align  (tupled prettyParams)
            <+> pretty "->"
            <+> pretty (returnT fType)  -- why does this compile?
            <+> pretty fBody
        where
            prettyParams = zipWith prettyParamAndType fParams $ paramT fType
            prettyParamAndType a t = pretty a <> colon <+> pretty t


instance Pretty n => Pretty (Program n) where
    -- TODO: can use mapM?
    pretty Program {..} = vsep $ (pretty <$> globals) <> ((<> semi) . pretty <$> funcs)

