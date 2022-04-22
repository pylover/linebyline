module HackLine.Parser where

import Data.List

import HackLine.Tokenizer
import HackLine.Helpers

data Exp
  = Void
  | Literal String
  | Func String [Exp]
  | Infix Exp String Exp
  | Eval [Exp]
  deriving (Eq, Show)

instance Dumper Exp where
  dump (Literal x) = x
  dump (Func x xs) = "(" ++ x ++ " " ++ intercalate " " (dump <$> xs) ++ ")"
  dump (Infix a x b) = "(" ++ dump a ++ " " ++ x ++ " " ++ dump b ++ ")"

functions :: [String]
functions = 
  [ "print"
  ]

infixes :: [String]
infixes = 
  [ "+"
  ]

parse :: Token String -> Exp
parse Empty = Void
parse (Only a)
  | a `elem` functions = Func a []
  | otherwise = Literal a
parse (Group [(Only a), (Only b), c]) 
  | b `elem` infixes = Infix (parse (Only a)) b (parse c) 
  | a `elem` functions = Func a [parse (Only b), parse c]
  | otherwise = Eval $ parse <$> [Only a, Only b, c]
parse (Group ((Only x):xs)) 
  | x `elem` functions = Func x $ parse <$> xs
  | otherwise =  Eval $ parse <$> ((Only x) : xs) 
