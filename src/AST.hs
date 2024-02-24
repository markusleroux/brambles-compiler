module AST where

-- https://smunix.github.io/www.stephendiehl.com/llvm/index.html
-- https://blog.josephmorag.com/posts/mcc1/

data Op
    = Add
    | Sub
    | Mult
    | Div
    deriving (Eq, Ord, Show)

type Name = String

data Expr
    = IntLit Integer
    | FloatLit Double
    | Var Name
    | BinOp Op Expr Expr
    | Call Name [Expr]
    | Assignment Expr Expr
    deriving (Eq, Ord, Show)

data Type
    = TyInt
    | TyFloat
    deriving (Eq, Ord, Show)

data Bind = Bind 
    { bindType :: Type
    , bindName :: Expr
    }
    deriving (Eq, Ord, Show)

data Function = Function
    { functionName :: Name
    , functionReturnType :: Type
    , functionArguments :: [Bind]
    , functionLocals :: [Bind]
    , functionBody :: [Expr]
    }
    deriving (Eq, Ord, Show)

