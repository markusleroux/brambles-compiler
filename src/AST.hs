module AST where

data UnOp
    = Neg
    | Pos
    deriving (Eq, Ord, Show)


data BinOp
    = Add
    | Sub
    | Mult
    | Div
    deriving (Eq, Ord, Show)


data Type
    = TInt
    | TFloat
    deriving (Eq, Ord, Show)


type Name = String

data Variable
    = UntypedVar Name
    | TypedVar Type Name
    deriving (Eq, Ord, Show)

data Expr
    = IntLit Integer
    | FloatLit Double
    | Variable Variable
    | UnOp UnOp Expr
    | BinOp BinOp Expr Expr
    | Call Name [Expr]
    | Assignment Variable Expr
    deriving (Eq, Ord, Show)

newtype Block = Block [Expr]
    deriving (Eq, Ord, Show)


data Function = Function
    { functionName :: Name
    , functionArguments :: [Variable]
    , functionReturnType :: Type
    , functionBody :: Block
    }
    deriving (Eq, Ord, Show)

data Program = Program
    { globals :: [Expr]
    , functions :: [Function]
    }
    deriving (Eq, Ord, Show)
