module Main where


import System.IO
import System.Console.Haskeline

import HackLine.Helpers
import HackLine.CLI
import HackLine.Evaluator


main :: IO ()
main = parseArgs >>= greet 


greet :: Args -> IO ()
greet (Args a) = runInputT defaultSettings (loop e)
  where e = eval $ spacer a


loop :: (String -> String) -> InputT IO ()
loop e = do
  minput <- getInputLine ""
  case minput of
    Nothing -> return ()
    Just input -> outputStrLn (e input) >> loop e
