module HackLine.Evaluator where

import HackLine.Helpers
import HackLine.Tokenizer

data Session = Session { index :: Int, args :: [String] }

evaluate :: [String] -> Session -> [String]
evaluate (x:xs) s = x : xs

eval :: String -> String -> String 
eval e i = spacer $  evaluate (tokenize e) (Session 0 [i])
