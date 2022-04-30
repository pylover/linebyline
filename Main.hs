module Main where


import HackLine.CLI


main :: IO ()
main = parseArgs >>= greet 


greet :: Args -> IO ()
greet (Args script) = putStrLn script
