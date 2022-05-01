module HackLine.Parser where

import Data.List

import HackLine.Helpers
import HackLine.Functions
import HackLine.Tokenizer


data Exp
  = Void
  | Var String
  | Literal String
  | Group [Exp]
  | Func String [Exp]
  | Pipe Exp Exp
  deriving (Eq, Show)


instance Semigroup Exp where
  a <> Void = a
  Void <> a = a

  a <> (Pipe as e) = Pipe (a <> as) e
  (Pipe e as) <> a = Pipe e (as <> a)

  (Func x xs) <> (Group gs) = Func x (xs ++ gs)
  (Func x xs) <> a = Func x (xs ++ [a])

  a <> (Group xs) = Group $ a : xs
  (Group xs) <> a = Group $ xs ++ [a]

  a <> b = Group [a, b]


instance Monoid Exp where
  mempty = Void


eat :: [String] -> (Exp, [String])
eat [] = (mempty, [])
eat ("(":xs) = (parse_ xp, ps)
  where (xp, ps) = splitByParenthesis xs
eat ("::":xs) = (Pipe Void Void, xs)
eat ((':':v):xs) = (Var v, xs)
eat (('\'':v):xs) = (Literal (init v), xs)
eat (x:xs) 
  | x `elem` functions = (Func x [], xs)
  | otherwise = (Literal x, xs)


parse_ :: [String] -> Exp
parse_ [] = mempty
parse_ xs = exp <> parse_ rs
  where (exp, rs) = eat xs


parse :: String -> Exp
parse = parse_.tokenize
