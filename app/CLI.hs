module CLI (Args(..), parseArgs) where


import Data.Version (showVersion)
import Options.Applicative

import Paths_linebyline (version)


data Args = Args
  { script :: [String]
  }


versionParser :: Parser (a -> a)
versionParser = infoOption (showVersion version)
  (  long "version"
  <> short 'V'
  <> help "Print version information" )


scriptParser :: Parser [String]
scriptParser = many ( strArgument (
   ( metavar "SCRIPT..."
  <> help "Line by line script to execute line-by-line on input." )))


appInfo :: InfoMod s
appInfo = fullDesc
       <> progDesc ""
       <> header "Line-by-line column based text editor"


parserInfo :: ParserInfo Args
parserInfo = info ( Args 
                <$> scriptParser 
               <**> versionParser 
               <**> helper) appInfo


parseArgs :: IO Args
parseArgs = customExecParser (prefs helpShowGlobals) parserInfo
