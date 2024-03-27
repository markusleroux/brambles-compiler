{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty where

import AST
import Parser ()
import Data.Maybe (maybeToList)
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
    pretty Add  = pretty "+"
    pretty Sub  = pretty "-"
    pretty Mult = pretty "*"
    pretty Div  = pretty "/"
    pretty Eq   = pretty "=="

instance Pretty Type where
    pretty TInt   = pretty "int"
    pretty TFloat = pretty "float"
    pretty TBool  = pretty "bool"
    pretty TUnit  = undefined
    pretty TCallable{..} = align (tupled $ pretty <$> paramT) <+> pretty "->" <+> pretty returnT

instance Pretty n => Pretty (Var n) where
    pretty (V v) = pretty v

instance Pretty n => Pretty (Block n 'Parsed) where
  pretty Block{..} = prettyLikeBlock $ prettyStmts ++ prettyBlockResults
    where
      prettyStmts = pretty <$> blockBody
      prettyBlockResults = maybeToList (pretty <$> blockResult)

instance Pretty n => Pretty (Expr n 'Parsed) where
    pretty EIntLit{..}   = pretty intLitVal
    pretty EFloatLit{..} = pretty floatLitVal
    pretty EBoolLit{..}  = pretty (if boolLitVal then "true" else "false")
    pretty EVar{..}      = pretty varVar
    pretty EBlock{..}    = pretty unBlock
    pretty EUnOp{..}     = pretty unOp <> prettyGrouped unRHS
    pretty EBinOp{..}    = prettyGrouped binLHS <+> pretty binOp <+> prettyGrouped binRHS
    pretty ECall{..}     = pretty callName <> align (tupled $ pretty <$> callArgs)
    pretty EAssign{..}   = pretty assignVar <+> equals <+> pretty assignExpr
    pretty EIf{..}       = pretty "if" <+> pretty ifPred <+> pretty "then" <+> pretty ifThen <> maybe mempty prettyElse ifElseMb
      where
        prettyElse b = pretty " else" <+> pretty b
    pretty EFunc{..} =
        pretty "fn"
            <+> pretty funcName
                <> align (tupled prettyParams)
            <+> pretty "->"
            <+> pretty (returnT $ snd funcX)
            <+> pretty funcBody
      where
        prettyParams = zipWith prettyParamAndType funcParams $ paramT (snd funcX)
        prettyParamAndType a t = pretty a <> colon <+> pretty t

prettyGrouped :: Pretty n => Expr n 'Parsed -> Doc ann
prettyGrouped lit@EIntLit{}   = pretty lit
prettyGrouped lit@EFloatLit{} = pretty lit
prettyGrouped var@EVar{}      = pretty var
prettyGrouped call@ECall{}    = pretty call
prettyGrouped p = pretty "(" <> pretty p <> pretty ")"

instance Pretty n => Pretty (Stmt n 'Parsed) where
    pretty SExpr{..}   = pretty exprExpr <> semi
    pretty SDecl{..}   = pretty "let" <+> pretty declName <> colon <+> pretty (snd declX) <+> equals <+> pretty declExpr <> semi
    pretty SWhile{..}  = pretty "while" <+> pretty whilePred <+> pretty whileBody <> semi
    pretty SReturn{..} = pretty "return" <+> pretty returnExpr <> semi

prettyLikeBlock :: [Doc ann] -> Doc ann
prettyLikeBlock l = braces . newlineIf (null l) . vsep $ l
  where
    newlineIf = bool (enclose line line) id

instance Pretty n => Pretty (Prog n 'Parsed) where
    pretty (Globals _ g) = vsep (pretty <$> g)
