module HackLine.Helpers where

import Data.List

splitPar :: Int -> [String] -> [String] -> ([String], [String])
splitPar _ left [] = (left, [])
splitPar i left ("(":xs) = splitPar (i + 1) (left ++ ["("]) xs
splitPar 0 left (")":xs) = (left, xs)
splitPar i left (")":xs) = splitPar (i - 1) (left ++ [")"]) xs
splitPar i left (x:xs) = splitPar i (left ++ [x]) xs

splitByParenthesis :: [String] -> ([String], [String])
splitByParenthesis = splitPar 0 []

spacer :: [String] -> String
spacer xs = intercalate " " xs

(+?) :: String -> [String] -> [String]
(+?) "" xs = xs
(+?) a xs = a : xs

quote :: String -> String
quote = decorate '\''

decorate :: Char -> String -> String
decorate c s  = c : s ++ [c]
