module Helpers 
  ( spacer
  , quote
  , matchParenthesis
  , strReplace
  , unEscape
  , capitalize
  )  where


import Data.Char (toUpper)
import Data.List
import Data.List.Split
import Text.Regex.TDFA


closingPar :: Int -> [String] -> [String] -> ([String], [String])
closingPar _ left [] = (left, [])
closingPar i left ("(":xs) = closingPar (i + 1) (left ++ ["("]) xs
closingPar 0 left (")":xs) = (left, xs)
closingPar i left (")":xs) = closingPar (i - 1) (left ++ [")"]) xs
closingPar i left (x:xs) = closingPar i (left ++ [x]) xs


matchParenthesis :: [String] -> ([String], [String])
matchParenthesis = closingPar 0 []


spacer :: [String] -> String
spacer xs = intercalate " " xs


quote :: String -> String
quote = decorate '\''


decorate :: Char -> String -> String
decorate c s  = c : s ++ [c]


unEscape :: String -> String
unEscape s = case break (=='\\') s of
  (a, (_:x:xs)) -> a ++ [x] ++ unEscape xs
  (a, _) -> a


strReplace :: String -> String -> String -> String
strReplace p r "" = ""
strReplace p r s = case s =~ p of
  (b, "", _) -> b
  (b, x, a) -> b ++ r ++ (strReplace p r a)


capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs
