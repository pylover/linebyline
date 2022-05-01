module Main where


import System.IO
-- import System.Console.Haskeline

import HackLine.Helpers
import HackLine.CLI


main :: IO ()
main = parseArgs >>= greet 


greet :: Args -> IO ()
greet (Args s) = putStrLn $ spacer s

-- greet _ = runInputT defaultSettings loop


-- loop :: InputT IO ()
-- loop = do
--   minput <- getInputLine ""
--   case minput of
--     Nothing -> return ()
--     Just input -> outputStrLn input >> loop
