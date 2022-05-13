module Evaluator 
   ( Signal(..)
   , eval
   , evaluate
   ) where


import Text.Read (readMaybe)
import Control.Monad.State (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, gets, put)

import Context
import Parser (Exp(..), parse)
import Functions (Signal(..), getFunc, CtxT(..) )


getArg :: Int -> CtxT String
getArg i = do
  args_ <- gets args
  if i < (length args_)
    then return $ args_ !! i
    else return $ ":" ++ show i


evalVar :: String -> CtxT String
evalVar "n" = show <$> gets index
evalVar "l" = gets line
evalVar n = case readMaybe n of
  Just x -> getArg x
  Nothing -> return $ ':' : n


evalSliceVar :: String -> String -> CtxT [String]
evalSliceVar "" "" = gets args
evalSliceVar a "" = case readMaybe a of
  Just x -> drop x <$> gets args
  Nothing -> return [":" ++ a ++ "~"]
evalSliceVar "" a = case readMaybe a of
  Just x -> take (x + 1) <$> gets args
  _ -> return [":~" ++ a]
evalSliceVar a b = case readMaybe <$> [a, b] of
  [Just x, Just y] -> (drop x) . (take (y + 1)) <$> gets args
  _ -> return [":" ++ a ++ "~" ++ b]


evaluate :: Exp -> CtxT [String]
evaluate Void = return []
evaluate (Literal a) = return [a]
evaluate (Var a) = case break (=='~') a of
  (l, "") -> (:[]) <$> evalVar l
  (l, _:r) -> evalSliceVar l r
evaluate (Group xs) = sequence (evaluate <$> xs) >>= return . mconcat
evaluate (Func f xs) = evaluate (Group xs) >>= getFunc f
evaluate (Pipe a b) = do
  ra <- evaluate a
  i <- gets index
  l <- gets line
  put $ Ctx i l ra
  evaluate b
evaluate (After a b) = do
  c <- get 
  ra <- evaluate a
  put c
  rb <- evaluate b
  return $ ra ++ rb 


eval :: String -> Int -> String -> Either Signal String 
eval e i a = evalStateT getExp getCtx >>= return . unwords
  where 
    getExp = evaluate (parse e)
    getCtx = Ctx i a [a]
