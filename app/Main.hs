module Main where

import Parser

import Text.Parsec (parse)
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)

process :: String -> IO ()
process line = do
    case parse programP "<stdin>" line of
        Left err -> print err
        Right prog -> print prog


main :: IO ()  -- REPL
main = runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "womp> "
            case minput of
                Nothing     -> return ()
                Just input  -> (liftIO $ process input) >> loop
