module Main where


import System.IO
import System.Exit
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Helpers
import CLI
import Evaluator


getScript :: [String] -> String
getScript [] = ":~"
getScript s = spacer s


main :: IO ()
main = parseArgs >>= evaluator >>= runMaybeT . loop 1 >> return ()
  where 
    evaluator (Args s) = return $ eval (getScript s)
    loop i e = readLine >>= process >>= reloop
      where 
        process = liftEval i . e i 
        reloop ni = loop ni e


liftEval :: Int -> Either Signal String -> MaybeT IO Int
liftEval i (Right x) = do
  liftIO $ putStrLn x
  return (i + 1)
liftEval i (Left SuppressLine) = return i
liftEval i (Left SuppressAll) = liftIO exit >> return i


liftMaybe :: Maybe a -> MaybeT IO a
liftMaybe = MaybeT . pure


readLine :: MaybeT IO String
readLine = do
  isClosed <- lift isEOF
  if isClosed 
    then mzero
    else lift getLine


exit :: IO ()
exit = exitWith ExitSuccess
