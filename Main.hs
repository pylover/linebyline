module Main where


import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import HackLine.Helpers
import HackLine.CLI
import HackLine.Evaluator


type Evaluator = Int -> String -> Maybe String


main :: IO ()
main = parseArgs >>= e >>= runMaybeT . loop 1 >> return ()
  where 
    e :: Args -> IO Evaluator
    e (Args s) = return $ eval (spacer s)


loop :: Int -> Evaluator -> MaybeT IO ()
loop i e = readLine >>= liftMaybe . e i >>= liftIO . putStrLn >> loop (i+1) e
