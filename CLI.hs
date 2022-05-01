module HackLine.CLI (Args(..), parseArgs) where


import Data.Version (showVersion)
import Options.Applicative

import Paths_hackline (version)


data Args = Args
  { script :: [String]
  }


versionParser :: Parser (a -> a)
versionParser = infoOption (showVersion version)
  (  long "version"
  <> short 'V'
  <> help "Print version information" )

parser :: Parser Args
parser = Args
  <$> many ( strArgument (
     ( metavar "SCRIPT..."
    <> help "Hackline script to execute line-by-line on input." )))
  -- <*> switch
  --    ( long "quiet"
  --   <> short 'q'
  --   <> help "Whether to be quiet" )
  -- <*> option auto
  --    ( long "enthusiasm"
  --   <> help "How enthusiastically to greet"
  --   <> showDefault
  --   <> value 1
  --   <> metavar "INT" )


options :: ParserInfo Args
options = info (parser <**> versionParser <**> helper)
   ( fullDesc
  <> progDesc "Foo Bar Baz"
  <> header "Qux" )


parseArgs :: IO Args
parseArgs = customExecParser (prefs helpShowGlobals) options
