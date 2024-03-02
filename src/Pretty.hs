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
    pretty TInt    = pretty "int"
    pretty TFloat  = pretty "float"

instance Pretty Variable where
    pretty (UntypedVar name)      = pretty name
    pretty (TypedVar type_ name)  = pretty type_ <+> pretty name


instance Pretty Expr where
    pretty (IntLit val)         = pretty val
    pretty (FloatLit val)       = pretty val
    pretty (Variable var)       = pretty var
    pretty (UnOp op exp)        = pretty op <> prettyGrouped exp
    pretty (BinOp op expl expr) = prettyGrouped expl <+> pretty op <+> prettyGrouped expr
    pretty (Call name exps)     = pretty name <> align (tupled $ pretty <$> exps)
    pretty (Assignment l r)     = pretty l <+> equals <+> pretty r
        
prettyGrouped lit@(IntLit _)   = pretty lit
prettyGrouped lit@(FloatLit _) = pretty lit
prettyGrouped var@(Variable _) = pretty var
prettyGrouped call@(Call _ _)  = pretty call
prettyGrouped p                = pretty "(" <> pretty p <> pretty ")"

instance Pretty Block where
    pretty (Block exprs) = braces $ multilineMb (null exprs) $ vsep $ prettyStatement <$> exprs
        where 
            multilineMb = bool (enclose line line) id

prettyStatement :: Pretty a => a -> Doc ann
prettyStatement = (<> semi) . pretty

instance Pretty Function where
    pretty fn = pretty "fn"
            <+> pretty (functionName fn)
             <> align  (tupled $ pretty <$> functionArguments fn)
            <+> pretty "->"
            <+> pretty (functionReturnType fn)
            <+> pretty (functionBody fn)


instance Pretty Program where
    pretty (Program exprs functions) = vsep $ (prettyStatement <$> exprs) <> (prettyStatement <$> functions)

