module Parser.Prop where

import qualified AST
import Hedgehog
import Parser (blockP, exprP, functionP, programP, typeP)
import Parser.Gen
import Pretty
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
        , testProperty "blockP" $ prop_prettyParserInverse blockP genBlock
        , testProperty "functionP" $ prop_prettyParserInverse functionP genFunction
        , testProperty "programP" $ prop_prettyParserInverse programP genProgram
        ]
