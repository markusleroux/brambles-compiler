module Main where

import Brambles.REPL.CLI
import qualified Brambles.Frontend.AST as AST
import Brambles.Frontend.Parser (exprP)
import Brambles.Frontend.Typecheck (runTypechecking, inferExpr, TypeError)
import qualified Brambles.Backend.Codegen as Codegen

import Control.Monad.Except
import System.Console.Haskeline (defaultSettings, getInputLine, runInputT)
import Text.Parsec (parse, ParseError)


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
      prompt   = getInputLine "brambles> "
      printers = fromOptions options
      quit     = putStrLn "quit"
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
  = ParseError ParseError
  | TypecheckError TypeError
  | CodegenError Codegen.CodegenError
  deriving (Show)

runAndPrintErrors :: Printers -> String -> IO ()
runAndPrintErrors p input = 
  let
    run :: Printers -> String -> ExceptT REPLError IO ()
    run Printers{..} line = do
      parsedAST <- getParsed line

      lift $ putStrLn "\nAST (parsed)"
      lift $ putStrLn "---------------------------"
      runPrinter astPrinter parsedAST

      typecheckedAST <- getTyped parsedAST

      lift $ putStrLn "\nAST (typed)"
      lift $ putStrLn "---------------------------"
      runPrinter typedAstPrinter typecheckedAST

      compiledModule <- getCompiled typecheckedAST
      lift $ Codegen.optimize compiledModule
      pure ()
      -- runPrinter irPrinter compiledModule
      -- runPrinter compilePrinter compiledModule
  in
    runExceptT (run p input) >>= either print pure
  where
    runPrinter pMb v = liftIO . sequence_ $ pMb <*> pure v

    getParsed   = withExceptT ParseError     . liftEither . parse exprP "<stdin>"
    getTyped    = withExceptT TypecheckError . liftEither . runTypechecking . inferExpr
    getCompiled = withExceptT CodegenError   . liftEither . Codegen.toLLVM

