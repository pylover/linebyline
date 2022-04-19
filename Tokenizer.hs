{-# LANGUAGE InstanceSigs #-}
module HackLine.Tokenizer where

import Data.List
import Data.List.NonEmpty hiding (tail, init)

class Dumper a where
  dump :: a -> String

data Token a 
  = Empty
  | Only a
  | Group (NonEmpty (Token a))
  deriving (Eq, Show)

instance Show a => Dumper (Token a) where
  dump Empty = ""
  dump (Only a) = tail.init $ (show a)
  dump (Group (x :| [])) = dump x
  dump (Group xs) = "(" ++ intercalate " " (dump <$> toList xs) ++ ")"

instance Semigroup (Token a) where
  Empty    <> x         = x
  x        <> Empty     = x
  Group xs <> Group ys  = Group (xs <> ys)
  x        <> Group xs  = Group $ x <| xs
  Group (x :| xs) <> y  = Group $ x :| xs <> [y]
  x        <> y         = Group (x :| [y])

instance Monoid (Token a) where 
  mempty = Empty 

mPar :: Int -> String -> String -> (String, String)
mPar _ left "" = (left, [])
mPar i left ('(':xs) = mPar (i + 1) (left ++ "(") xs
mPar 0 left (')':xs) = (left, xs)
mPar i left (')':xs) = mPar (i - 1) (left ++ ")") xs
mPar i left (x:xs) = mPar i (left ++ [x]) xs

tokenize :: String -> String -> Token String
tokenize "" "" = Empty
tokenize "" (' ':xs) = tokenize "" xs
tokenize cur "" = Only cur
tokenize cur (' ':xs) = Only cur <> tokenize "" xs
tokenize cur ('(':xs) = 
  (tokenize cur "") <> (Group ((tokenize "" xp) :| [])) <> (tokenize "" ps)
  where (xp, ps) = mPar 0 "" xs
tokenize cur (x:xs) = tokenize (cur ++ [x]) xs
