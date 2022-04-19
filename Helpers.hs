module HackLine.Helpers where

mPar :: Int -> String -> String -> (String, String)
mPar _ left "" = (left, [])
mPar i left ('(':xs) = mPar (i + 1) (left ++ "(") xs
mPar 0 left (')':xs) = (left, xs)
mPar i left (')':xs) = mPar (i - 1) (left ++ ")") xs
mPar i left (x:xs) = mPar i (left ++ [x]) xs

class Dumper a where
  dump :: a -> String
