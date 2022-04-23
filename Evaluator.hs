module HackLine.Evaluator where

import HackLine.Helpers
import HackLine.Tokenizer
import HackLine.Parser

data Ctx = Ctx { index :: Int, args :: [String] }

evaluate :: Ctx -> [String] -> Exp -> [String]
evaluate _ _ (Literal a) = [a]
evaluate c a (Group xs) = mconcat $ evaluate c a <$> xs
-- evaluate c a (Func f xs) = mconcat $ evaluate c a <$> xs

eval :: String -> String -> String 
eval e i = spacer $ evaluate (Ctx 0 [i]) [i] (parse e)
