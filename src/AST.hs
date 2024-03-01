module AST where

import Prettyprinter
import Data.Bool

data UnOp
    = Neg
    | Pos
    deriving (Eq, Ord, Show)

instance Pretty UnOp where
    pretty Neg = pretty "-"
    pretty Pos = pretty "+"


data BinOp
    = Add
    | Sub
    | Mult
    | Div
    deriving (Eq, Ord, Show)

instance Pretty BinOp where
    pretty Add  = pretty "+"
    pretty Sub  = pretty "-"
    pretty Mult = pretty "*"
    pretty Div  = pretty "/"


data Type
    = TInt
    | TFloat
    deriving (Eq, Ord, Show)

instance Pretty Type where
    pretty TInt    = pretty "int"
    pretty TFloat  = pretty "float"


type Name = String

data Variable
    = UntypedVar Name
    | TypedVar Type Name
    deriving (Eq, Ord, Show)

instance Pretty Variable where
    pretty (UntypedVar name)      = pretty name
    pretty (TypedVar type_ name)  = pretty type_ <+> pretty name


data Expr
    = IntLit Integer
    | FloatLit Double
    | Variable Variable
    | UnOp UnOp Expr
    | BinOp BinOp Expr Expr
    | Call Name [Expr]
    | Assignment Variable Expr
    deriving (Eq, Ord, Show)

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

newtype Block = Block [Expr]
    deriving (Eq, Ord, Show)

instance Pretty Block where
    pretty (Block exprs) = braces $ multilineMb (null exprs) $ vsep $ prettyStatement <$> exprs
        where 
            multilineMb = bool (enclose line line) id

prettyStatement :: Pretty a => a -> Doc ann
prettyStatement = (<> semi) . pretty


data Function = Function
    { functionName :: Name
    , functionArguments :: [Variable]
    , functionReturnType :: Type
    , functionBody :: Block
    }
    deriving (Eq, Ord, Show)

instance Pretty Function where
    pretty fn = pretty "fn"
            <+> pretty (functionName fn)
             <> align  (tupled $ pretty <$> functionArguments fn)
            <+> pretty "->"
            <+> pretty (functionReturnType fn)
            <+> pretty (functionBody fn)


data Program = Program
    { globals :: [Expr]
    , functions :: [Function]
    }
    deriving (Eq, Ord, Show)

instance Pretty Program where
    pretty (Program exprs functions) = vsep $ (prettyStatement <$> exprs) <> (prettyStatement <$> functions)

