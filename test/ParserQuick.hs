module ParserQuick where

import AST
import Parser

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

import Prettyprinter
import Prettyprinter.Render.String

import Control.Monad (liftM2, liftM3)

instance Arbitrary AST.Op where
    arbitrary = elements [ Add, Sub, Mult, Div ]

instance Arbitrary AST.Type where
    arbitrary = elements [ TInt, TFloat ]

instance Arbitrary AST.Variable where
    arbitrary = QC.oneof
        [ UntypedVar <$> arbitraryIdentifier
        , liftM2 TypedVar arbitrary arbitraryIdentifier
        ]
        where
            arbitraryIdentifier = listOf1 $ elements ['a'..'z']  -- TODO: more identifiers

instance Arbitrary AST.Expr where
    -- TODO: restrict size of tree
    arbitrary = QC.oneof
        [ IntLit <$> arbitrary
        , FloatLit <$> arbitrary
        , Variable <$> arbitrary
        , liftM3 BinOp arbitrary arbitrary arbitrary
        , liftM2 Call arbitrary $ QC.listOf arbitrary
        , liftM2 Assignment arbitrary arbitrary
        ]

prop_prettyParserInverse :: (Show a, Pretty a, Eq a, Arbitrary a) => Parser a -> a -> Property
prop_prettyParserInverse parser ast =
    let 
        prettyAST = renderString . layoutPretty defaultLayoutOptions . pretty $ ast
    in
        case parse parser "" prettyAST of
            Left err -> (QC.counterexample $ "Failed to compile:\n" ++ prettyAST ++ "\n" ++ show err) $ property False
            Right a -> ast === a


parsingQuickTests = testGroup "Pretty followed by parse is identify"
    [ QC.testProperty "typeP" $ prop_prettyParserInverse typeP
    , QC.testProperty "variableP" $ prop_prettyParserInverse variableP
    , QC.testProperty "exprP" $ prop_prettyParserInverse exprP
    ]
