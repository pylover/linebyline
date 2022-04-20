module HackLine.Parser where

import Data.List

import HackLine.Tokenizer
import HackLine.Helpers

data Exp
  = Literal String
  | Func String [Exp]
  | Infix Exp String Exp
  deriving (Eq, Show)

instance Dumper Exp where
  dump (Literal x) = x
  dump (Func x xs) = x ++ " " ++ intercalate " " (dump <$> xs)
  dump (Infix a x b) = dump a ++ " " ++ x ++ " " ++ dump b

functions :: [String]
functions = ["print"]

parse :: Token String -> Token Exp
parse Empty = Empty
parse (Only a)
  | a `elem` functions = Only $ Func a []
  | otherwise = Only $ Literal a
-- tree :: [String] -> Exp
-- tree a:b:xs
--   | a `elem` functions = Func a (eatArgs b:xs)
--   | b `elem` infixes = Infix (tree a) b (tree xs)
--  
-- eatArgs :: [String] -> [Exp]
-- eatArgs ('(':xs) = tree 
-- eatArgs (x:xs) = tree [x] : eatArgs xs
-- 
-- parse :: String -> Exp
-- parse x = tree $ words x
