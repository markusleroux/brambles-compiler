module Main where

import Protolude hiding (TypeError)
import qualified Prelude (Show(..))

import Brambles.REPL.CLI
import qualified Brambles.Frontend.AST as AST
import Brambles.Frontend.Parser (exprP)
import Brambles.Frontend.Typecheck (runTypechecking, inferExpr, TypeError)
import qualified Brambles.Backend.ASTToLLVM as ASTToLLVM
import qualified Brambles.Backend.Codegen as Codegen

import Control.Monad.Except (liftEither)
import System.Console.Haskeline (defaultSettings, getInputLine, runInputT)
import Text.Megaparsec (parse, errorBundlePretty, ParseErrorBundle)


main :: IO () -- REPL
main = getREPLOptions >>= repl

repl :: REPLOptions -> IO () -- Execute
repl options =
    let 
      promptAndRun = prompt >>= \case
          Nothing    -> liftIO quit
          Just input -> liftIO (run input) >> promptAndRun
     in 
      runInputT defaultSettings promptAndRun
     where 
      prompt   = toS <<$>> getInputLine "brambles> "
      printers = fromOptions options
      quit     = putText "quit"
      run      = runAndPrintErrors printers

data Printers = Printers
    { astPrinter      :: Maybe (AST.Expr AST.Name 'AST.Parsed -> IO ())
    , typedAstPrinter :: Maybe (AST.Expr AST.Name 'AST.Typed -> IO ())
    , irPrinter       :: Maybe (Codegen.Module -> IO ())
    , compilePrinter  :: Maybe (Codegen.Module -> IO ())
    }

fromOptions :: REPLOptions -> Printers
fromOptions REPLOptions{..} =
    Printers
        { astPrinter      = Just print
        , typedAstPrinter = if typedAST then Just print else Nothing
        , irPrinter       = Nothing -- if ir then Just (TIO.putStrLn . Codegen.ppllvm) else Nothing
        , compilePrinter  = Nothing -- flip Codegen.compile <$> compile
        }


data REPLError
  = ParseError (ParseErrorBundle Text Void)
  | TypecheckError TypeError
  | ASTToLLVMError ASTToLLVM.ASTToLLVMError

instance Show REPLError where
  show (ParseError e)     = errorBundlePretty e
  show (TypecheckError e) = show e
  show (ASTToLLVMError e)   = show e


runAndPrintErrors :: Printers -> Text -> IO ()
runAndPrintErrors p input = 
  let
    run :: Printers -> Text -> ExceptT REPLError IO ()
    run Printers{..} line = do
      parsedAST <- getParsed line

      lift $ putText "\nAST (parsed)"
      lift $ putText "---------------------------"
      runPrinter astPrinter parsedAST

      typecheckedAST <- getTyped parsedAST

      lift $ putText "\nAST (typed)"
      lift $ putText "---------------------------"
      runPrinter typedAstPrinter typecheckedAST

      compiledModule <- getCompiled typecheckedAST
      _ <- lift $ Codegen.optimize compiledModule
      pure ()
      -- runPrinter irPrinter compiledModule
      -- runPrinter compilePrinter compiledModule
  in
    runExceptT (run p input) >>= either print pure
  where
    runPrinter pMb v = liftIO . sequence_ $ pMb <*> pure v

    getParsed   = withExceptT ParseError     . liftEither . parse exprP "<stdin>"
    getTyped    = withExceptT TypecheckError . liftEither . runTypechecking . inferExpr
    getCompiled = withExceptT ASTToLLVMError . liftEither . ASTToLLVM.runExprtoLLVM

