module HackLine.Parser where

import Data.Foldable

import HackLine.Helpers
import HackLine.Functions
import HackLine.Tokenizer


data Exp
  = Void
  | Var String
  | Literal String
  | Group [Exp]
  | Func String [Exp]
  | Pipe Exp String Exp
  deriving (Eq, Show)


instance Semigroup Exp where
  a <> Void = a
  Void <> a = a

  a <> (Pipe Void p e) = Pipe a p e
  (Pipe e p Void) <> a = Pipe e p a

  (Func x xs) <> (Group gs) = Func x (xs ++ gs)
  (Func x xs) <> a = Func x (xs ++ [a])

  a <> (Group xs) = Group $ a : xs
  (Group xs) <> a = Group $ xs ++ [a]

  a <> b = Group [a, b]


instance Monoid Exp where
  mempty = Void


parse_ :: [String] -> Exp
parse_ [] = Void
parse_ ("(":xs) = parse_ xp <> parse_ ps
  where (xp, ps) = splitByParenthesis xs
parse_ (['>', '>', x]:xs) = Pipe Void ['>', '>', x] (parse_ xs)
parse_ (x:xs) 
  | x `elem` functions = Func x [] <> parse_ xs
  | otherwise = Literal x <> parse_ xs


parse :: String -> Exp
parse = parse_.tokenize
