module Helpers 
  ( spacer
  , quote
  , matchParenthesis
  , strReplace
  )  where


import Data.List
import Data.List.Split
import Text.Regex.TDFA


splitPar :: Int -> [String] -> [String] -> ([String], [String])
splitPar _ left [] = (left, [])
splitPar i left ("(":xs) = splitPar (i + 1) (left ++ ["("]) xs
splitPar 0 left (")":xs) = (left, xs)
splitPar i left (")":xs) = splitPar (i - 1) (left ++ [")"]) xs
splitPar i left (x:xs) = splitPar i (left ++ [x]) xs


matchParenthesis :: [String] -> ([String], [String])
matchParenthesis = splitPar 0 []


spacer :: [String] -> String
spacer xs = intercalate " " xs


quote :: String -> String
quote = decorate '\''


decorate :: Char -> String -> String
decorate c s  = c : s ++ [c]


strReplace :: String -> String -> String -> String
strReplace p r "" = ""
strReplace p r s = case s =~ p of
  (b, "", _) -> b
  (b, x, a) -> b ++ r ++ (strReplace p r a)
