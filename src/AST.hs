module AST where


data Op
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
    | BinOp Op Expr Expr
    | Call Name [Expr]
    | Assignment Expr Expr
    deriving (Eq, Ord, Show)

data Block = Block [Expr]
    deriving (Eq, Ord, Show)

data Function = Function
    { functionName :: Name
    , functionArguments :: [Variable]
    , functionReturnType :: Type
    , functionBody :: Block
    }
    deriving (Eq, Ord, Show)

