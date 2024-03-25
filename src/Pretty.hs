{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty where

import AST
import Parser (SourceLoc)
import Data.Bool (bool)
import Prettyprinter (
    Doc,
    Pretty (pretty),
    align,
    braces,
    colon,
    enclose,
    equals,
    line,
    semi,
    tupled,
    vsep,
    (<+>),
 )
import Prelude hiding (exp)

-- TODO:
-- proper formatting
-- command line access

instance Pretty UnOp where
    pretty Neg = pretty "-"
    pretty Pos = pretty "+"

instance Pretty BinOp where
    pretty Add = pretty "+"
    pretty Sub = pretty "-"
    pretty Mult = pretty "*"
    pretty Div = pretty "/"
    pretty Eq = pretty "=="

instance Pretty Type where
    pretty TInt = pretty "int"
    pretty TFloat = pretty "float"
    pretty TBool = pretty "bool"
    pretty TUnit = undefined
    pretty TCallable{..} = align (tupled $ pretty <$> paramT) <+> pretty "->" <+> pretty returnT

instance Pretty n => Pretty (Var n) where
    pretty (V v) = pretty v

instance Pretty n => Pretty (Expr n 'Parsed) where
    pretty (EIntLit _ val) = pretty val
    pretty (EFloatLit _ val) = pretty val
    pretty (EBoolLit _ val) = if val then pretty "true" else pretty "false"
    pretty (EVar _ var) = pretty var
    pretty (EBlock _ block) = pretty block
    pretty EUnOp{..} = pretty unOp <> prettyGrouped unRHS
    pretty EBinOp{..} = prettyGrouped binLHS <+> pretty binOp <+> prettyGrouped binRHS
    pretty ECall{..} = pretty callFunc <> align (tupled $ pretty <$> callArgs)
    pretty EAssign{..} = pretty assignVar <+> equals <+> pretty assignVal
    pretty EIf{..} = pretty "if" <+> pretty ifCond <> pretty "then" <+> pretty ifBody <> elseBody
      where
        elseBody = case ifElseMb of
            Just b -> pretty " else" <+> pretty b
            Nothing -> mempty

prettyGrouped :: Pretty n => Expr n 'Parsed -> Doc ann
prettyGrouped lit@(EIntLit _ _) = pretty lit
prettyGrouped lit@(EFloatLit _ _) = pretty lit
prettyGrouped var@(EVar _ _) = pretty var
prettyGrouped call@(ECall{}) = pretty call
prettyGrouped p = pretty "(" <> pretty p <> pretty ")"

instance Pretty n => Pretty (Stmt n 'Parsed) where
    pretty (SExpr _ exp) = pretty exp <> semi
    pretty SDecl{..} = pretty "let" <+> pretty declName <> colon <+> pretty (snd declExt) <+> equals <+> pretty declV <> semi
    pretty SWhile{..} = pretty "while" <+> pretty whileCond <+> pretty whileBody <> semi
    pretty (SReturn _ exp) = pretty "return" <+> pretty exp <> semi
    pretty SFunc{..} =
        pretty "fn"
            <+> pretty fName
                <> align (tupled prettyParams)
            <+> pretty "->"
            <+> pretty (returnT $ snd fExt)
            <+> pretty fBody
                <> semi
      where
        prettyParams = zipWith prettyParamAndType fParams $ paramT (snd fExt)
        prettyParamAndType a t = pretty a <> colon <+> pretty t

instance Pretty n => Pretty (Block n 'Parsed) where
    pretty (Block _ stmts) = braces . newlineIf (null stmts) . vsep $ pretty <$> stmts
      where
        newlineIf = bool (enclose line line) id

instance Pretty n => Pretty (Prog n 'Parsed) where
    pretty (Globals _ g) = vsep (pretty <$> g)
