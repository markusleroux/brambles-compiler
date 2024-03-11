module ParserQuick where

import AST
import Data.Either (isRight)
import Lexer (identifier)
import Parser
import Pretty
import Prettyprinter
import Prettyprinter.Render.String
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Text.Parsec (parse)
import Text.Parsec.String (Parser)

instance Arbitrary AST.UnOp where
    arbitrary = elements [Neg, Pos]

instance Arbitrary AST.BinOp where
    arbitrary = elements [Add, Sub, Mult, Div]

instance Arbitrary AST.Type where
    arbitrary = elements [TInt, TFloat]

arbitraryIdentifier = listOf1 (elements ['_', 'a' .. 'z']) `QC.suchThat` canParse identifier
  where
    canParse :: Parser a -> String -> Bool
    canParse p = isRight . parse p ""

instance Arbitrary (AST.Expr Name) where
    arbitrary = sized ast
      where
        ast 0 =
            QC.oneof
                [ IntLit . abs <$> arbitrary
                , FloatLit . abs <$> arbitrary
                , Var <$> arbitraryIdentifier
                ]
        ast n =
            QC.oneof
                [ UnOp <$> arbitrary <*> ast (n - 1)
                , BinOp <$> arbitrary <*> halfAst <*> halfAst
                , Call <$> arbitraryIdentifier <*> listOfScaled arbitrary
                , Assign <$> arbitraryIdentifier <*> ast (n - 1)
                , EBlock <$> arbitrary
                ]
          where
            halfAst = ast (n `div` 2 :: Int)

listOfScaled :: Arbitrary a => Gen a -> QC.Gen [a]
listOfScaled s = do
    n <- getSize
    k <- QC.choose (0, n)
    QC.vectorOf k $ resize (n `div` k :: Int) s

instance Arbitrary (AST.Stmt Name) where
    arbitrary =
        QC.oneof
            [ Expr <$> arbitrary
            , Decl <$> arbitraryIdentifier <*> arbitrary <*> arbitrary
            ]

instance Arbitrary (AST.Block Name) where
    arbitrary = Block <$> listOfScaled arbitrary

instance Arbitrary (AST.Func Name) where
    arbitrary =
        arbitrarySizedNatural >>= \n ->
            Func <$> arbitraryIdentifier
                <*> vectorOf n arbitraryIdentifier
                <*> (TCallable <$> vectorOf n arbitrary <*> arbitrary)
                <*> arbitrary

instance Arbitrary (AST.Program Name) where
    arbitrary = Program <$> arbitrary <*> arbitrary

prop_prettyParserInverse :: (Show a, Pretty a, Eq a, Arbitrary a) => Parser a -> a -> Property
prop_prettyParserInverse parser ast =
    let prettyAST = renderString . layoutPretty defaultLayoutOptions . pretty $ ast
     in within 1000000 $ case parse parser "" prettyAST of
            Left err -> QC.counterexample ("Failed to compile:\n" ++ prettyAST ++ "\n" ++ show err) $ property False
            Right a -> ast === a

parsingQuickTests =
    -- TODO: how to do this properly?
    adjustOption (const $ QC.QuickCheckMaxSize 5) $
        adjustOption (const $ QC.QuickCheckTests 1000) $
            testGroup
                "Pretty followed by parse is identify"
                [ QC.testProperty "typeP" $ prop_prettyParserInverse typeP
                , QC.testProperty "exprP" $ prop_prettyParserInverse exprP
                , QC.testProperty "blockP" $ prop_prettyParserInverse blockP
                , QC.testProperty "functionP" $ prop_prettyParserInverse functionP
                , QC.testProperty "programP" $ prop_prettyParserInverse programP
                ]
