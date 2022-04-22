module HackLine.Parser where

import Data.List

import HackLine.Tokenizer
import HackLine.Helpers
import HackLine.Functions

data Exp
  = Void
  | Literal String
  | Func String [Exp]
  deriving (Eq, Show)

instance Dumper Exp where
  dump Void = "()"
  dump (Literal x) = x
  dump (Func x xs) = "(" ++ x ++ " " ++ intercalate " " (dump <$> xs) ++ ")"

parseToken :: Token String -> Exp
parseToken Empty = Void
parseToken (Only a)
  | a `elem` functions = Func a []
  | otherwise = Literal a
parseToken (Group ((Only x):xs)) 
  | x `elem` functions = Func x $ parseToken <$> xs
  | otherwise =  Func "print" $ parseToken <$> ((Only x) : xs) 
parseToken (Group xs) = Func "print" $ parseToken <$> xs

parse :: String -> Exp
parse s = parseToken $ tokenize "" s
