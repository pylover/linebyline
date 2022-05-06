module Tokenizer ( tokenize ) where


import Helpers (quote)


tokenize_ :: String -> String -> [String]

-- Empty input
tokenize_ c "" = c : []

-- Escape char
tokenize_ w@('\\':cs) (x:xs) = tokenize_ (x : w) xs

-- Space
tokenize_ c (' ':xs) = c : tokenize_ "" xs

-- Single Quote
tokenize_ c ('\'':xs) = c : quote (reverse xq) : tokenize_ "" qs
  where (xq, (_:qs)) = break (=='\'') xs

-- ::: After
tokenize_ c (':':':':':':xs) = c : (":::" : tokenize_ "" xs)

-- :: Pipe
tokenize_ c (':':':':xs) = c : ("::" : tokenize_ "" xs)

-- Func | Literal
tokenize_ c (x:xs) 
  | x `elem` "()" = c : ([x] : tokenize_ "" xs)
  | otherwise = tokenize_ (x : c) xs


tokenize :: String -> [String]
tokenize s = filter (/="") $ reverse <$> tokenize_ "" s
