module Pretty where

import Prettyprinter
import AST

import Data.Bool

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
        

prettyGrouped lit@(IntLit _)   = pretty lit
prettyGrouped lit@(FloatLit _) = pretty lit
prettyGrouped var@(Var _)      = pretty var
prettyGrouped call@(Call _ _)  = pretty call
prettyGrouped p                = pretty "(" <> pretty p <> pretty ")"

instance Pretty n => Pretty (Stmt n) where
    pretty (Expr exp) = pretty exp <> semi
    pretty (Decl name t exp) = pretty "let" 
                           <+> pretty name
                            <> colon
                           <+> pretty t
                           <+> equals
                           <+> pretty exp
                            <> semi

instance Pretty n => Pretty (Block n) where
    pretty (Block stmts) = braces . encloseIf (null stmts) . vsep $ pretty <$> stmts
        where 
            encloseIf = bool (enclose line line) id

instance Pretty n => Pretty (Func n) where
    pretty fn = pretty "fn"
            <+> pretty (fName fn)
             <> align  (tupled prettyParams)
            <+> pretty "->"
            <+> pretty (returnT . fType $ fn)  -- why does this compile?
            <+> pretty (fBody fn)
        where
            prettyParams = zipWith (\a t -> pretty a <> colon <+> pretty t) (fParams fn) (paramT . fType $ fn)


instance Pretty n => Pretty (Program n) where
    -- TODO: can use mapM?
    pretty (Program stmts functions) = vsep $ (pretty <$> stmts) <> ((<> semi) . pretty <$> functions)

