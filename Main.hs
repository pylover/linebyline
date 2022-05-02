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


liftEither :: Either Signal String -> MaybeT IO String
liftEither (Right x) = return x
liftEither (Left SuppressLine) = mzero
liftEither (Left SuppressAll) = liftIO exit >> mzero


loop :: Int -> Evaluator -> MaybeT IO ()
loop i e = readLine >>= liftEither . e i >>= liftIO . putStrLn >> loop (i+1) e
