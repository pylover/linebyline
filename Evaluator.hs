module HackLine.Evaluator 
   ( Signal(..)
   , eval
   , evaluate
   , evaluator
   , Evaluator(..)
   ) where


import Text.Read

import HackLine.Helpers
import HackLine.Context
import HackLine.Tokenizer
import HackLine.Parser
import HackLine.Functions


type Evaluator = Int -> String -> Either Signal String


getArg :: Ctx -> Int -> String
getArg (Ctx _ _ xs) i 
  | i < l = xs !! i
  | otherwise = ":" ++ show i
  where l = length xs


evalVar :: Ctx -> String -> String
evalVar c "n" = show $ index c
evalVar c "l" = line c
evalVar c n = case readMaybe n of
  Just a -> getArg c a
  Nothing -> ':' : n


evalSliceVar :: Ctx -> String -> String -> [String]
evalSliceVar c "" "" = args c
evalSliceVar c a "" = case readMaybe a of
  Just x -> drop x (args c) 
  Nothing -> [":" ++ a ++ "~"]
evalSliceVar c "" a = case readMaybe a of
  Just x -> take (x + 1) (args c)
  _ -> [":~" ++ a]
evalSliceVar c a b = case readMaybe <$> [a, b] of
  [Just x, Just y] -> drop x $ take (y + 1) $ args c
  _ -> [":" ++ a ++ "~" ++ b]


evalGroup :: Ctx -> [Exp] -> [String] -> Either Signal [String]
evalGroup c [] r = Right r
evalGroup c (x:xs) r = case evaluate c x of
  Right rs -> evalGroup c xs (r ++ [spacer rs])
  ls -> ls


evaluate :: Ctx -> Exp -> Either Signal [String]
evaluate c Void = Right $ args c
evaluate _ (Literal a) = Right [a]
evaluate c (Var a) = case break (=='~') a of
  (l, "") -> Right [evalVar c l]
  (l, _:r) -> Right $ evalSliceVar c l r
evaluate c (Group xs) = evalGroup c xs []
evaluate c (Func f xs) = evaluate c (Group xs) >>= (getFunc f) c
evaluate c (Pipe a b) = evaluate c a >>= nb
  where nb x = evaluate (Ctx (index c) (line c) x) b


eval :: String -> Int -> String -> Either Signal String 
eval e i a = evaluate (Ctx i a [a]) (parse e) >>= Right . spacer


evaluator :: String -> Evaluator
evaluator = eval
