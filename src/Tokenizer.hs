module Tokenizer 
  ( tokenize
  ) where


import Helpers ((+?), quote)


tokenize_ :: String -> String -> [String]
tokenize_ c "" = c +? []
tokenize_ ('\\':cs) (x:xs) = tokenize_ (x : '\\' : cs) xs
tokenize_ c (' ':xs) = c +? tokenize_ "" xs
tokenize_ c ('\'':xs) = c +? (quote (reverse xq) +? tokenize_ "" qs)
  where (xq, (_:qs)) = break (=='\'') xs
tokenize_ c (':':':':':':xs) = c +? (":::" : tokenize_ "" xs)
tokenize_ c (':':':':xs) = c +? ("::" : tokenize_ "" xs)
tokenize_ c (x:xs) 
  | x `elem` "()" = c +? ([x] : tokenize_ "" xs)
  | otherwise = tokenize_ (x : c) xs


tokenize :: String -> [String]
tokenize s = reverse <$> tokenize_ "" s
