{-# LANGUAGE InstanceSigs #-}
module HackLine.Tokenizer where

(+?) :: String -> [String] -> [String]
(+?) "" xs = xs
(+?) a xs = a : xs


tokenize_ :: String -> String -> [String]
tokenize_ c "" = c +? []
tokenize_ c (' ':xs) = c +? tokenize_ "" xs
tokenize_ c ('>':'>':'+':xs) = c +? (">>+" : tokenize_ "" xs)
tokenize_ c ('>':'>':'=':xs) = c +? (">>=" : tokenize_ "" xs)
tokenize_ c ('>':'>':xs) = c +? (">>" : tokenize_ "" xs)
tokenize_ c (x:xs) 
  | x `elem` "()" = c +? ([x] : tokenize_ "" xs)
  | otherwise = tokenize_ (c ++ [x]) xs

tokenize :: String -> [String]
tokenize = tokenize_ ""

