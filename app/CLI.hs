module CLI where

import Options.Applicative hiding (ParseError)

data REPLOptions = REPLOptions
    { typedAST :: !Bool
    , ir :: !Bool
    , compile :: Maybe FilePath
    }

getREPLOptions :: IO REPLOptions
getREPLOptions =
    execParser $
        info (helper <*> commandLine) fullDesc

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

