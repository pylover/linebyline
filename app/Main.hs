module Main where


import System.IO 
  ( Handle
  , hGetLine
  , hIsEOF
  , IOMode(..)
  , openFile
  , stdin
  , hClose
  )
import System.Exit (ExitCode(..), exitWith)

import Helpers
import CLI
import Evaluator


getScript :: [String] -> String
getScript [] = ":~"
getScript s = unwords s


getFile :: String -> IO Handle
getFile "-" = return stdin
getFile x = openFile x ReadMode


type Eval = Int -> String -> Either Signal String


main :: IO ()
main = do
  (Args inps s) <- parseArgs 
  process (e s) inps
  return ()
  where 
    e x = eval $ getScript x


process :: Eval -> [String] -> IO Int
process e [] = process e ["-"]
process e xs = loopFiles e xs 1


loopFiles :: Eval -> [String] -> Int -> IO Int
loopFiles e [] i = return i
loopFiles e (f:fx) i = do
  h <- getFile f
  j <- loopLines e h i >>= loopFiles e fx
  hClose h
  return j


loopLines :: Eval -> Handle -> Int -> IO Int
loopLines e h i = do
  isClosed <- hIsEOF h
  if isClosed
    then return i
    else do
      l <- hGetLine h
      case e i l of 
        Left SuppressLine -> loopLines e h i
        Left SuppressAll -> exit >> return i
        Right r -> putStrLn r >> loopLines e h (i + 1)


exit :: IO ()
exit = exitWith ExitSuccess
