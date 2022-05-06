module Main where


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
main = parseArgs >>= e >>= runMaybeT . loop 1 >> return ()
  where 
    e (Args s) = return $ eval (getScript s)
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
