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
    | TCallable {paramT :: [Type], returnT :: Type}
    deriving (Eq, Ord, Show)

type Name = String

data Expr n
    = IntLit Integer
    | FloatLit Double
    | Var n
    | UnOp UnOp (Expr n)
    | BinOp BinOp (Expr n) (Expr n)
    | Call n [Expr n]
    | Assign n (Expr n)
    | EBlock (Block n)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Stmt n
    = Expr (Expr n)
    | Decl {sName :: n, sType :: Type, sVal :: Expr n}
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Block n = Block {unBlock :: [Stmt n]}
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Func n = Func
    { fName :: n
    , fParams :: [n]
    , fType :: Type
    , fBody :: Block n
    }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Program n = Program
    { globals :: [Stmt n]
    , funcs :: [Func n]
    }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
