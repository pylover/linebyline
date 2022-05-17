module Main where


import System.IO 
  ( Handle
  , hGetLine
  , hIsEOF
  , IOMode(..)
  , openFile
  , stdin
  , stdout
  , hClose
  , hPutStrLn
  , hFlush
  , withFile
  )
import System.Exit (ExitCode(..), exitWith)
import Control.Monad.State (lift)
import Control.Monad.Trans.State 
  ( StateT
  , modify'
  , gets
  , evalStateT)

import Helpers
import CLI
import Evaluator


type Eval = Int -> String -> Either Signal String
data EvalState = EvalState {line :: Int, evaluator :: Eval, outfile :: Handle}
type EvalStateT a = StateT EvalState IO a


main :: IO ()
main = do
  (Args inps out s) <- parseArgs 
  withOutFile out (dodo inps s)
  where 
    dodo i s o = (evalStateT (process i) (EvalState 1 (eval_ s) o))
    eval_ :: [String] -> Eval
    eval_ [] = eval_ [":~"]
    eval_ xs = eval $ unwords xs

    process :: [String] -> EvalStateT ()
    process [] = process ["-"]
    process xs = loopFiles xs
  
    withOutFile "-" f = f stdout
    withOutFile fn f = withFile fn WriteMode f


loopFiles :: [FilePath] -> EvalStateT ()
loopFiles [] = return ()
loopFiles (f:fx) = withInputFile f loopLines >> loopFiles fx


withInputFile :: FilePath -> (Handle -> EvalStateT ()) -> EvalStateT () 
withInputFile fn f = do
  h <- getFile fn
  f h
  lift $ hClose h
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
          o <- gets outfile
          lift $ hPutStrLn o r
          modify' nextLine
          loopLines h


nextLine :: EvalState -> EvalState
nextLine (EvalState l e o) = EvalState (l + 1) e o


exit :: IO ()
exit = exitWith ExitSuccess
