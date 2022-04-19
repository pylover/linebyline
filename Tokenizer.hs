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

tokenize :: String a => a -> a -> Token a
tokenize "" "" = Empty
tokenize "" (' ':xs) = tokenize "" xs
tokenize cur "" = Only cur
tokenize cur (' ':xs) = Only cur <> tokenize "" xs
tokenize cur (x:xs) = tokenize (cur ++ [x]) xs


