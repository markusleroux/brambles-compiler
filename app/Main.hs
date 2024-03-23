module Main where

import Parser (programP, exprP)
import qualified AST

import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import Text.Parsec (parse)
import LLVM.Pretty (ppllvm)
import qualified Data.Text.Lazy.IO as TIO

import LLVM
import Options.Applicative

data REPLOptions = REPLOptions
  { ir :: !Bool
  , compile :: Maybe FilePath
  }

main :: IO () -- REPL
main = getREPLOptions >>= repl

getREPLOptions :: IO REPLOptions
getREPLOptions = execParser $ 
  info (helper <*> commandLine) fullDesc

repl :: REPLOptions -> IO () -- Execute
repl options = 
  let
    printers = fromOptions options
    run = runPrinters printers

    parseAndRun line = case parse exprP "<stdin>" line of
        Left err -> print err
        Right prog -> run prog

    promptAndRun = getInputLine "womp> " >>= \case
      Nothing -> return ()
      Just input -> liftIO (parseAndRun input) >> promptAndRun
  in
    runInputT defaultSettings promptAndRun


commandLine :: Parser REPLOptions
commandLine = REPLOptions
  <$> printIRParser
  <*> optional doCompileParser

printIRParser :: Parser Bool
printIRParser = switch 
   $ long "ir" 
  <> short 'i' 
  <> help "Print the IR to stdout"

doCompileParser :: Parser FilePath
doCompileParser = strOption
   $ long "output" 
  <> short 'o' 
  <> help "Compile output to file"


data Printers = Printers
  { astPrinter :: Maybe (AST.Expr AST.Name -> IO ())
  , irPrinter :: Maybe (AST.Expr AST.Name -> IO ())
  , compilePrinter :: Maybe (AST.Expr AST.Name -> IO ())
  }

fromOptions :: REPLOptions -> Printers
fromOptions REPLOptions{..} = Printers
  { astPrinter = Just print
  , irPrinter = if ir then Just (TIO.putStrLn . ppllvm . toLLVM) else Nothing
  , compilePrinter = flip (LLVM.compile . toLLVM) <$> compile
  }

runPrinters :: Printers -> AST.Expr AST.Name -> IO ()
runPrinters Printers{..} e = do
  sequence $ astPrinter <*> pure e
  sequence $ irPrinter <*> pure e
  sequence $ compilePrinter <*> pure e
  pure ()

