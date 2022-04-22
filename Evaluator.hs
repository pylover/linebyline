module HackLine.Evaluator where

import HackLine.Parser

data Session = Session { lineNo :: Int, args :: [String]}

funcPrint :: Session -> [String] -> [String]
funcPrint _ xs = xs

getFunc :: String -> (Session -> [String] -> [String])
getFunc "print" = funcPrint

evalExp :: Session -> Exp -> [String]
evalExp s (Func f fargs) = (getFunc f) s (mconcat $ (evalExp s) <$> fargs)
evalExp _ (Literal a) = [a]
evalExp _ Void = []

eval :: String -> [String] -> [String]
eval e a = evalExp (Session 0 a) (parse e)
