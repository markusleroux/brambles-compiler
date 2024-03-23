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

data Options = Options
  { ir :: Bool
  }

commandLine :: Parser Options
commandLine = Options
  <$> switch (long "ir" <> short 'i' <> help "Print the IR to stdout")

data Printers = Printers
  { astPrinter :: Maybe (AST.Expr AST.Name -> IO ())
  , irPrinter :: Maybe (AST.Expr AST.Name -> IO ())
  }

fromCommandLine :: Options -> Printers
fromCommandLine Options{..} = Printers
  { astPrinter = Just print
  , irPrinter = if ir then Just (TIO.putStrLn . ppllvm . toLLVM) else Nothing
  }

runPrinters :: Printers -> AST.Expr AST.Name -> IO ()
runPrinters Printers{..} e = do
  sequence $ astPrinter <*> pure e
  sequence $ irPrinter <*> pure e
  pure ()


doProcessing :: Printers -> String -> IO ()
doProcessing p line = do
    case parse exprP "<stdin>" line of
        Left err -> print err
        Right prog -> runPrinters p prog

main :: IO () -- REPL
main = do
  options <- execParser $ info commandLine fullDesc
  runInputT defaultSettings $ loop (fromCommandLine options)
    where
      loop p = getInputLine "womp> " >>= \case
        Nothing -> return ()
        Just input -> liftIO (doProcessing p input) >> loop p
