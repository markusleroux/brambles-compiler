module AST where

import Prettyprinter

data Op
    = Add
    | Sub
    | Mult
    | Div
    deriving (Eq, Ord, Show)

instance Pretty Op where
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
    | BinOp Op Expr Expr
    | Call Name [Expr]
    | Assignment Expr Expr
    deriving (Eq, Ord, Show)

instance Pretty Expr where
    pretty (IntLit val)         = pretty val
    pretty (FloatLit val)       = pretty val
    pretty (Variable var)       = pretty var
    pretty (BinOp op expl expr) = pretty expl <+> pretty op <+> pretty expr
    pretty (Call name exps)     = pretty name <> align (tupled $ pretty <$> exps)
    pretty (Assignment l r)     = pretty l <+> equals <+> pretty r


newtype Block = Block [Expr]
    deriving (Eq, Ord, Show)

instance Pretty Block where
    pretty (Block exprs) = braces $ vsep $ (<> semi) . pretty <$> exprs


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

