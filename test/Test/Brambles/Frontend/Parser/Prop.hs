module Test.Brambles.Frontend.Parser.Prop where

import qualified Brambles.Frontend.AST
import Brambles.Frontend.Parser (exprP, programP, statementP, typeP)
import Brambles.Frontend.Pretty
import Test.Brambles.Frontend.Parser.Gen

import Hedgehog
import Prettyprinter
import Prettyprinter.Render.String
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Text.Parsec (parse)
import Text.Parsec.String (Parser)

prop_prettyParserInverse :: (Show a, Pretty a, Eq a) => Parser a -> Gen a -> Property
prop_prettyParserInverse parser g = property $ do
    ast <- forAll g
    tripping ast printer parser'
  where
    printer = renderString . layoutPretty defaultLayoutOptions . pretty
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