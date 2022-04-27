module HackLine.Helpers where

import Data.List
import Data.String


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


findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)


split :: String -> String -> String -> [String]
split c n "" = [c]
split c n (s:ss)= case stripPrefix n (s:ss) of
  Just x -> c : split "" n x
  Nothing -> split (c ++ [s]) n ss
