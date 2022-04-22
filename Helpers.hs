module HackLine.Helpers where

import Data.List

mPar :: Int -> String -> String -> (String, String)
mPar _ left "" = (left, [])
mPar i left ('(':xs) = mPar (i + 1) (left ++ "(") xs
mPar 0 left (')':xs) = (left, xs)
mPar i left (')':xs) = mPar (i - 1) (left ++ ")") xs
mPar i left (x:xs) = mPar i (left ++ [x]) xs

spacer :: [String] -> String
spacer xs = intercalate " " xs
