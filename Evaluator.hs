module HackLine.Evaluator (eval, evaluate) where


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
  | otherwise = ":" ++ show i
  where l = length xs


evalVar :: Ctx -> String -> String
evalVar (Ctx i _) "n" = show i
evalVar c n = case readMaybe n of
  Just a -> getArg c a
  Nothing -> ':' : n


evaluate :: Ctx -> Exp -> [String]
evaluate _ (Literal a) = [a]
evaluate c (Var a) = [evalVar c a]
evaluate c (Group xs) = (spacer . evaluate c) <$> xs
evaluate c (Func f xs) = evalFunc c f aa
  where aa = evaluate c (Group xs)
evaluate (Ctx i ca) (Pipe a b) = evaluate (Ctx i na) b
  where na = evaluate (Ctx i ca) a


eval :: String -> Int -> String -> String 
eval e i a = spacer $ evaluate (Ctx i [a]) (parse e)
