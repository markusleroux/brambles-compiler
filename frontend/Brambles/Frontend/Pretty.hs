{-# OPTIONS_GHC -fno-warn-orphans #-}

module Brambles.Frontend.Pretty where

import Protolude hiding (Type)


import Brambles.Frontend.AST
import Brambles.Frontend.Parser ()


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

-- TODO:
-- proper formatting
-- command line access

instance Pretty UnOp where
    pretty Neg = pretty ("-" :: Text)
    pretty Pos = pretty ("+" :: Text)

instance Pretty BinOp where
    pretty Add  = pretty ("+" :: Text)
    pretty Sub  = pretty ("-" :: Text)
    pretty Mult = pretty ("*" :: Text)
    pretty Div  = pretty ("/" :: Text)
    pretty Eq   = pretty ("==" :: Text)

instance Pretty Type where
    pretty TInt   = pretty ("int" :: Text)
    pretty TFloat = pretty ("float" :: Text)
    pretty TBool  = pretty ("bool" :: Text)
    pretty TUnit  = pretty ("()" :: Text)
    pretty TCallable{..} = align (tupled $ pretty <$> paramT) <+> pretty ("->" :: Text) <+> pretty returnT
    pretty (TOptional t) = pretty ("Maybe" :: Text) <+> pretty t

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
    pretty EBoolLit{..}  = pretty (if boolLitVal then "true" else "false" :: Text)
    pretty EVar{..}      = pretty varVar
    pretty EBlock{..}    = pretty unBlock
    pretty EUnOp{..}     = pretty unOp <> prettyGrouped unRHS
    pretty EBinOp{..}    = prettyGrouped binLHS <+> pretty binOp <+> prettyGrouped binRHS
    pretty ECall{..}     = pretty callName <> align (tupled $ pretty <$> callArgs)
    pretty EAssign{..}   = pretty assignVar <+> equals <+> pretty assignExpr
    pretty EIf{..}       = pretty ("if" :: Text) 
                       <+> pretty ifPred 
                       <+> pretty ("then" :: Text)
                       <+> pretty ifThen 
                        <> maybe mempty prettyElse ifElseMb
      where
        prettyElse b = pretty (" else" :: Text) <+> pretty b
    pretty EFunc{..} = pretty unFunc

instance Pretty n => Pretty (Func n 'Parsed) where
    pretty Func{..} =
        pretty ("fn" :: Text)
            <+> pretty funcName
                <> align (tupled prettyParams)
            <+> pretty ("->" :: Text)
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
prettyGrouped p = pretty ("(" :: Text) <> pretty p <> pretty (")" :: Text)

instance Pretty n => Pretty (Stmt n 'Parsed) where
    pretty SExpr{..}   = pretty exprExpr <> semi
    pretty SDecl{..}   = pretty ("let" :: Text) 
                     <+> pretty declName 
                      <> colon 
                     <+> pretty (snd declX) 
                     <+> equals 
                     <+> pretty declExpr 
                     <> semi
    pretty SWhile{..}  = pretty ("while" :: Text) <+> pretty whilePred <+> pretty whileBody <> semi
    pretty SReturn{..} = pretty ("return" :: Text) <+> pretty returnExpr <> semi

prettyLikeBlock :: [Doc ann] -> Doc ann
prettyLikeBlock l = braces . newlineIf (null l) . vsep $ l
  where
    newlineIf = bool (enclose line line) identity

instance Pretty n => Pretty (Module n 'Parsed) where
    pretty Module{..} = vsep $ prettyGlobals ++ prettyFuncs
      where
        prettyGlobals = pretty <$> moduleGlobals
        prettyFuncs   = (\f -> pretty f <> semi) <$> moduleFuncs

