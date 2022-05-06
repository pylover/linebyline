module Helpers 
  ( split
  , spacer
  , quote
  , (+?)
  , splitByParenthesis
  , strReplace
  )  where


import Data.List
import Text.Regex.TDFA


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


split_ :: String -> String -> String -> [String]
split_ c n "" = [c]
split_ c n (s:ss)= case stripPrefix n (s:ss) of
  Just x -> c : split_ "" n x
  Nothing -> split_ (c ++ [s]) n ss


split :: String -> String -> [String]
split s xs = filter (/="") res
  where res = split_ "" s xs


strReplace :: String -> String -> String -> String
strReplace p r "" = ""
strReplace p r s = case s =~ p of
  (b, "", _) -> b
  (b, x, a) -> b ++ r ++ (strReplace p r a)
