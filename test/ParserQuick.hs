module ParserQuick where

import AST
import Parser
import Lexer (identifier)

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

import Prettyprinter
import Prettyprinter.Render.String

import Control.Monad (liftM2, liftM3, liftM4)
import Data.Either (isRight)


instance Arbitrary AST.UnOp where
    arbitrary = elements [ Neg, Pos ]

instance Arbitrary AST.BinOp where
    arbitrary = elements [ Add, Sub, Mult, Div ]

instance Arbitrary AST.Type where
    arbitrary = elements [ TInt, TFloat ]

arbitraryIdentifier = (listOf1 $ elements ['_','a'..'z']) `QC.suchThat` (canParse identifier)
    where
        canParse :: Parser a -> String -> Bool
        canParse p s = isRight $ parse p "" s

instance Arbitrary AST.Variable where
    arbitrary = QC.oneof
        [ UntypedVar <$> arbitraryIdentifier
        , liftM2 TypedVar arbitrary arbitraryIdentifier
        ]

instance Arbitrary AST.Expr where
    arbitrary = sized ast
        where 
            ast 0 = QC.oneof
                [ IntLit . abs   <$> arbitrary
                , FloatLit . abs <$> arbitrary
                , Variable       <$> arbitrary
                ]
            ast n = QC.oneof
                [ liftM2 UnOp       arbitrary $ ast (n - 1)
                , liftM3 BinOp      arbitrary halfAst halfAst
                , liftM2 Call       arbitraryIdentifier listAst
                , liftM2 Assignment arbitrary $ ast (n - 1)
                ]
                where
                    halfAst = ast ( n `div` 2 :: Int )
                    listAst = do
                        k <- QC.choose (0, n)
                        QC.vectorOf k $ ast ( n `div` k :: Int )

instance Arbitrary AST.Block where
    arbitrary = Block <$> arbitrary

instance Arbitrary AST.Function where
    arbitrary = liftM4 Function arbitraryIdentifier arbitrary arbitrary arbitrary

instance Arbitrary AST.Program where
    arbitrary = liftM2 Program arbitrary arbitrary


prop_prettyParserInverse :: (Show a, Pretty a, Eq a, Arbitrary a) => Parser a -> a -> Property
prop_prettyParserInverse parser ast =
    let 
        prettyAST = renderString . layoutPretty defaultLayoutOptions . pretty $ ast
    in
         within 1000000 $ case parse parser "" prettyAST of
            Left err -> QC.counterexample ( "Failed to compile:\n" ++ prettyAST ++ "\n" ++ show err) $ property False
            Right a -> ast === a


parsingQuickTests 
    -- TODO: how to do this properly?
    = adjustOption (const $ QC.QuickCheckMaxSize 5) 
    $ adjustOption (const $ QC.QuickCheckTests 1000)
    $ testGroup "Pretty followed by parse is identify"
        [ QC.testProperty "typeP"     $ prop_prettyParserInverse typeP
        , QC.testProperty "variableP" $ prop_prettyParserInverse variableP
        , QC.testProperty "exprP"     $ prop_prettyParserInverse exprP
        , QC.testProperty "blockP"    $ prop_prettyParserInverse blockP
        , QC.testProperty "functionP" $ prop_prettyParserInverse functionP
        , QC.testProperty "programP"  $ prop_prettyParserInverse programP
        ]

