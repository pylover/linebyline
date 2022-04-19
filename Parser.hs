module HackLine.Parser where

import Data.List

data Tree
  = Value String
  | Func String [Tree]
  | Infix Tree String Tree

instance Show Tree where
  show (Value x) = x
  show (Func x xs) = x ++ " " ++ intercalate " " (show <$> xs)
  show (Infix a x b) = show a ++ " " ++ x ++ " " ++ show b

functions :: [String]
functions = ["print"]

-- matchParenthesis :: Int -> [String] -> [String] -> ([String], [String])
-- matchParenthesis _ left [] = [left, []]
-- matchParenthesis i left ('(':xs) = matchParenthesis (i + 1) (left ++ ["("]) xs
-- matchParenthesis 0 left (')':xs) = [left, xs]
-- matchParenthesis i left (')':xs) = matchParenthesis (i - 1) (left ++ [")"]) xs
-- matchParenthesis i left (x:xs) = matchParenthesis i (left ++ [x]) xs

-- tree :: [String] -> Tree
-- tree a:b:xs
--   | a `elem` functions = Func a (eatArgs b:xs)
--   | b `elem` infixes = Infix (tree a) b (tree xs)
--  
-- eatArgs :: [String] -> [Tree]
-- eatArgs ('(':xs) = tree 
-- eatArgs (x:xs) = tree [x] : eatArgs xs
-- 
-- parse :: String -> Tree
-- parse x = tree $ words x
