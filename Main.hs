module Main where

import Debug.Trace
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import HackLine.Helpers
import HackLine.CLI
import HackLine.Evaluator


main :: IO ()
main = parseArgs >>= e >>= runMaybeT . loop 1 >> return ()
  where 
    e :: Args -> IO Evaluator
    e (Args s) = return $ evaluator (spacer s)


liftEval :: Either Signal String -> MaybeT IO ()
liftEval (Right x) = liftIO $ putStrLn x
liftEval (Left SuppressLine) = return ()
liftEval (Left SuppressAll) = liftIO exit


loop :: Int -> Evaluator -> MaybeT IO ()
loop i e = readLine >>= liftEval . e i >> loop (i+1) e
