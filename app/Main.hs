module Main where

import qualified AST
import Parser (exprP, programP)
import Typecheck (runTypechecking, inferExpr, TypeError, Typechecking)
import qualified Codegen
import qualified Codegen as LLVM (ppllvm, Module, toLLVM)

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy.IO as TIO
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import Text.Parsec (parse, ParseError)

import Options.Applicative hiding (ParseError)

data REPLOptions = REPLOptions
    { typedAST :: !Bool
    , ir :: !Bool
    , compile :: Maybe FilePath
    }

main :: IO () -- REPL
main = getREPLOptions >>= repl

getREPLOptions :: IO REPLOptions
getREPLOptions =
    execParser $
        info (helper <*> commandLine) fullDesc

repl :: REPLOptions -> IO () -- Execute
repl options =
    let 
      promptAndRun = getInputLine "womp> " >>= \case
          Nothing -> liftIO $ putStrLn "quit"
          Just input -> liftIO (runAndPrintErrors printers input) >> promptAndRun
     in 
      runInputT defaultSettings promptAndRun
     where 
      printers = fromOptions options

commandLine :: Parser REPLOptions
commandLine =
    REPLOptions
        <$> printTypedASTParser
        <*> printIRParser
        <*> optional doCompileParser

printTypedASTParser :: Parser Bool
printTypedASTParser =
    switch $
        long "typed"
            <> short 't'
            <> help "Print the typed AST to stdout"


printIRParser :: Parser Bool
printIRParser =
    switch $
        long "ir"
            <> short 'i'
            <> help "Print the IR to stdout"

doCompileParser :: Parser FilePath
doCompileParser =
    strOption $
        long "output"
            <> short 'o'
            <> help "Compile output to file"

data Printers = Printers
    { astPrinter :: Maybe (AST.Expr AST.Name 'AST.Parsed -> IO ())
    , typedAstPrinter :: Maybe (AST.Expr AST.Name 'AST.Typed -> IO ())
    , irPrinter :: Maybe (LLVM.Module -> IO ())
    , compilePrinter :: Maybe (LLVM.Module -> IO ())
    }

fromOptions :: REPLOptions -> Printers
fromOptions REPLOptions{..} =
    Printers
        { astPrinter = Just print
        , typedAstPrinter = if typedAST then Just print else Nothing
        , irPrinter = if ir then Just (TIO.putStrLn . LLVM.ppllvm) else Nothing
        , compilePrinter = flip Codegen.compile <$> compile
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
      parsedAST <- withExceptT ParseError $
          liftEither $ parse exprP "<stdin>" line

      runPrinter astPrinter parsedAST

      let t :: Typechecking AST.Name (AST.Expr AST.Name 'AST.Typed) = inferExpr parsedAST
      typecheckedAST <- withExceptT TypecheckError $
          liftEither $ runTypechecking t
      runPrinter typedAstPrinter typecheckedAST

      compiledModule <- withExceptT CodegenError $
        liftEither $ LLVM.toLLVM typecheckedAST
      runPrinter irPrinter compiledModule
      runPrinter compilePrinter compiledModule
  in
    runExceptT (run p input) >>= either print pure
  where
    runPrinter pMb v = liftIO . sequence_ $ pMb <*> pure v

