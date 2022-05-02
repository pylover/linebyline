module Main where


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


liftEval :: Either Signal String -> MaybeT IO String
liftEval (Right x) = return x
liftEval (Left SuppressLine) = mzero
liftEval (Left SuppressAll) = liftIO exit >> mzero


loop :: Int -> Evaluator -> MaybeT IO ()
loop i e = readLine >>= liftEval . e i >>= liftIO . putStrLn >> loop (i+1) e
