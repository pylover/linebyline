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


data Signal = SuppressLine | SuppressAll
  deriving (Eq, Show)


type Evaluator = Int -> String -> Either Signal String


evalFunc :: Ctx -> String -> [String] -> Either Signal [String]
evalFunc c n a = Right $ (getFunc n) c a


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


evalGroup :: Ctx -> [Exp] -> [String] -> Either Signal [String]
evalGroup c [] r = Right r
evalGroup c (x:xs) r = case evaluate c x of
  Right rs -> evalGroup c xs (r ++ [spacer rs])
  ls -> ls


evaluate :: Ctx -> Exp -> Either Signal [String]
evaluate (Ctx _ ca) Void = Right ca
evaluate _ (Literal a) = Right [a]
evaluate c (Var a) = Right [evalVar c a]
evaluate c (Group xs) = evalGroup c xs []
evaluate c (Func f xs) = evaluate c (Group xs) >>= evalFunc c f
evaluate (Ctx i ca) (Pipe a b) = evaluate (Ctx i ca) a >>= nb
  where nb x = evaluate (Ctx i x) b


eval :: String -> Int -> String -> Either Signal String 
eval e i a = evaluate (Ctx i [a]) (parse e) >>= Right . spacer


evaluator :: String -> Evaluator
evaluator = eval
