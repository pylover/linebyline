module HackLine.Evaluator where

import HackLine.Functions
import HackLine.Parser

evalExp :: Session -> Exp -> [String]
evalExp s (Func f fargs) = (getFunc f) s (mconcat $ (evalExp s) <$> fargs)
evalExp _ (Literal a) = [a]
evalExp _ Void = []

eval :: String -> [String] -> [String]
eval e a = evalExp (Session 0 a) (parse e)
