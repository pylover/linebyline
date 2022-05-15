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
import Control.Monad.State (lift)
import Control.Monad.Trans.State (StateT, modify', gets, runStateT)

import Helpers
import CLI
import Evaluator


type Eval = Int -> String -> Either Signal String
data EvalState = EvalState {line :: Int, evaluator :: Eval}
type EvalStateT a = StateT EvalState IO a


main :: IO ()
main = do
  (Args inps s) <- parseArgs 
  runStateT (process inps) (EvalState 1 (eval_ s))
  return ()
  where 
    eval_ :: [String] -> Eval
    eval_ [] = eval_ [":~"]
    eval_ xs = eval $ unwords xs

    process :: [String] -> EvalStateT Int
    process [] = process ["-"]
    process xs = loopFiles xs


loopFiles :: [String] -> EvalStateT Int
loopFiles [] = gets line
loopFiles (f:fx) = do
  h <- getFile f
  loopLines h
  loopFiles fx
  lift $ hClose h
  gets line
  where
    getFile "-" = return stdin
    getFile x = lift $ openFile x ReadMode


loopLines :: Handle -> EvalStateT ()
loopLines h = do
  isClosed <- lift $ hIsEOF h
  if isClosed
    then return ()
    else do
      l <- lift $ hGetLine h
      e <- gets evaluator
      i <- gets line
      case e i l of 
        Left SuppressLine -> loopLines h
        Left SuppressAll -> lift exit
        Right r -> do
          lift $ putStrLn r
          modify' nextLine
          loopLines h


nextLine :: EvalState -> EvalState
nextLine (EvalState l e) = EvalState (l + 1) e


exit :: IO ()
exit = exitWith ExitSuccess
