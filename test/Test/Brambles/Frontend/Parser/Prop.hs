module Test.Brambles.Frontend.Parser.Prop where

import Protolude

import Brambles.Frontend.Parser (exprP, programP, statementP, typeP)
import Brambles.Frontend.Lexer (Parser)
import Brambles.Frontend.Pretty ()
import Test.Brambles.Frontend.Parser.Gen (genExpr, genProgram, genStmt, genType)

import Text.Megaparsec (parse)

import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, Pretty)
import Prettyprinter.Render.Text (renderStrict)

import Hedgehog (forAll, Gen, Property, property, tripping)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

prop_prettyParserInverse :: (Show a, Pretty a, Eq a) => Parser a -> Gen a -> Property
prop_prettyParserInverse parser g = property $ do
    ast <- forAll g
    tripping ast printer parser'
  where
    printer = renderStrict . layoutPretty defaultLayoutOptions . pretty
    parser' = parse parser ""

parsingQuickTests :: TestTree
parsingQuickTests =
    testGroup
        "Pretty followed by parse is identify"
        [ testProperty "typeP" $ prop_prettyParserInverse typeP genType
        , testProperty "exprP" $ prop_prettyParserInverse exprP genExpr
        , testProperty "statementP" $ prop_prettyParserInverse statementP genStmt
        , testProperty "programP" $ prop_prettyParserInverse programP genProgram
        ]
