module HackLine.Parser where

import Data.List

import HackLine.Tokenizer
import HackLine.Helpers

data Exp
  = Void
  | Literal String
  | Func String [Exp]
  deriving (Eq, Show)

instance Dumper Exp where
  dump Void = "()"
  dump (Literal x) = x
  dump (Func x xs) = "(" ++ x ++ " " ++ intercalate " " (dump <$> xs) ++ ")"

functions :: [String]
functions = 
  [ "eval"
  , "print"
  ]

parse :: Token String -> Exp
parse Empty = Void
parse (Only a)
  | a `elem` functions = Func a []
  | otherwise = Literal a
parse (Group ((Only x):xs)) 
  | x `elem` functions = Func x $ parse <$> xs
  | otherwise =  Func "eval" $ parse <$> ((Only x) : xs) 
parse (Group xs) = Func "eval" $ parse <$> xs
