module CLI (Args(..), parseArgs) where


import Data.Version (showVersion)
import Options.Applicative

import Paths_linebyline (version)


data Args = Args
  { inputs :: [String]
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


inputsParser :: Parser [String]
inputsParser = many (strOption
  ( long "input"
 <> short 'i'
 <> metavar "FILENAME"
 <> help "Input file. otherwise standard input will be choosen. this option\
         \can be specified multiple times."))


appInfo :: InfoMod s
appInfo = fullDesc
       <> progDesc ""
       <> header "Line-by-line column based text editor"


args :: Parser Args
args = Args 
   <$> inputsParser 
   <*> scriptParser 
      


parseArgs :: IO Args
parseArgs = customExecParser (prefs helpShowGlobals) opts
  where opts = info (args <**> versionParser <**> helper) appInfo
