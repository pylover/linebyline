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
getArg (Ctx _ xs) i 
  | i < l = xs !! i
  | otherwise = ":" ++ show i
  where l = length xs


evalVar :: Ctx -> String -> String
evalVar (Ctx i _) "n" = show i
evalVar c n = case readMaybe n of
  Just a -> getArg c a
  Nothing -> ':' : n


evalSliceVar :: Ctx -> String -> String -> [String]
evalSliceVar (Ctx i xs) "" "" = xs
evalSliceVar (Ctx i xs) a "" = case readMaybe a of
  Just x -> drop x xs
  Nothing -> [":" ++ a ++ "~"]
evalSliceVar (Ctx i xs) "" a = case readMaybe a of
  Just x -> take (x + 1) xs
  _ -> [":~" ++ a]
evalSliceVar (Ctx i xs) a b = case readMaybe <$> [a, b] of
  [Just x, Just y] -> drop x $ take (y + 1) $ xs
  _ -> [":" ++ a ++ "~" ++ b]


evalGroup :: Ctx -> [Exp] -> [String] -> Either Signal [String]
evalGroup c [] r = Right r
evalGroup c (x:xs) r = case evaluate c x of
  Right rs -> evalGroup c xs (r ++ [spacer rs])
  ls -> ls


evaluate :: Ctx -> Exp -> Either Signal [String]
evaluate (Ctx _ ca) Void = Right ca
evaluate _ (Literal a) = Right [a]
evaluate c (Var a) = case break (=='~') a of
  (l, "") -> Right [evalVar c l]
  (l, _:r) -> Right $ evalSliceVar c l r
-- evaluate c (Var a) = Right [evalVar c a]
evaluate c (Group xs) = evalGroup c xs []
evaluate c (Func f xs) = evaluate c (Group xs) >>= (getFunc f) c
evaluate (Ctx i ca) (Pipe a b) = evaluate (Ctx i ca) a >>= nb
  where nb x = evaluate (Ctx i x) b


eval :: String -> Int -> String -> Either Signal String 
eval e i a = evaluate (Ctx i [a]) (parse e) >>= Right . spacer


evaluator :: String -> Evaluator
evaluator = eval
