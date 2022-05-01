module Main where


import System.IO
import System.Console.Haskeline

import HackLine.Helpers
import HackLine.CLI
import HackLine.Evaluator


main :: IO ()
main = parseArgs >>= greet 


greet :: Args -> IO ()
greet (Args a) = runInputT defaultSettings (loop e 1)
  where e = eval (spacer a)


loop :: (Int -> String -> String) -> Int -> InputT IO ()
loop e i = do
  minput <- getInputLine ""
  case minput of
    Nothing -> return ()
    Just input -> outputStrLn (e i input) >> loop e (i+1)
