module HackLine.Evaluator where

import Text.Read

import HackLine.Helpers
import HackLine.Context
import HackLine.Tokenizer
import HackLine.Parser
import HackLine.Functions


evalFunc :: Ctx -> String -> [String] -> [String]
evalFunc c n a = (getFunc n) c a 

getArg :: Ctx -> Int -> String
getArg (Ctx _ xs) i 
  | i < l = xs !! i
  | otherwise = ""
  where l = length xs

evalVar :: Ctx -> String -> String
evalVar (Ctx i _) "n" = show i
evalVar c n = case readMaybe n of
  Just a -> getArg c a
  Nothing -> ""

evaluate :: Ctx -> Exp -> [String]
evaluate _ (Literal a) = [a]
evaluate c (Var a) = [evalVar c a]
evaluate c (Group xs) = mconcat $ evaluate c <$> xs
evaluate (Ctx i ca) (Func f []) = evalFunc (Ctx i ca) f ca
evaluate c (Func f xs) = evalFunc c f $ evaluate c (Group xs)


eval :: String -> String -> String 
eval e i = spacer $ evaluate (Ctx 0 [i]) (parse e)
