module CLI (Args(..), parseArgs) where


import Data.Version (showVersion)
import Options.Applicative

import Paths_linebyline (version)


data Args = Args
  { inputs :: [FilePath]
  , output :: FilePath
  , script :: [String]
  }
  deriving Show

versionParser :: Parser (a -> a)
versionParser = infoOption (showVersion version)
  (  long "version"
  <> short 'V'
  <> help "Print version information" )


scriptParser :: Parser [String]
scriptParser = many ( strArgument (
   ( metavar "SCRIPT..."
  <> help "Line by line script to execute line-by-line on input." )))


inputsParser :: Parser [FilePath]
inputsParser = many (strOption
  ( long "input"
 <> short 'i'
 <> metavar "FILENAME"
 <> help "Input file. otherwise standard input will be choosen. this option\
        \ can be specified multiple times and you may use '-' for\
        \ standard input."))


outputParser :: Parser FilePath
outputParser = strOption
  ( long "output"
 <> short 'o'
 <> metavar "FILENAME"
 <> value "-"
 <> help "Output filename, default: standard output")


appInfo :: InfoMod s
appInfo = fullDesc
       <> progDesc ""
       <> header "Line-by-line column based text editor"


args :: Parser Args
args = Args 
   <$> inputsParser 
   <*> outputParser 
   <*> scriptParser 
      

parseArgs :: IO Args
parseArgs = customExecParser (prefs helpShowGlobals) opts
  where opts = info (args <**> versionParser <**> helper) appInfo
