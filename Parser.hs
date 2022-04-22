module HackLine.Parser where

import HackLine.Tokenizer

data Exp
  = Void
  | Var String
  | Literal String
  | Func String [Exp]
  deriving (Eq, Show)

parse_ :: [String] -> Exp
parse_ _ = Void

parse :: String -> Exp
parse = parse_.tokenize
