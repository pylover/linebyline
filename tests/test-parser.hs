{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Parser


main = htfMain htf_thisModulesTests


test_parse_literal = do
  assertEqual Void $ parse ""
  assertEqual (Literal "foo") $ parse "foo"
  assertEqual (Group [Literal "foo", Literal "bar"]) $ parse "foo bar"
  assertEqual (Group [Literal "foo", Literal "bar", Literal "baz"]) 
    $ parse "foo (bar baz)"


test_parse_pipe = do
  assertEqual (Pipe (Literal "foo") (Literal "bar")) 
    $ parse "foo >> bar"

  assertEqual (Pipe (Literal "foo") 
              (Pipe (Literal "bar") (Literal "baz"))) 
    $ parse "foo >> bar >> baz"

  assertEqual (Pipe (Group [Literal "foo", Literal "bar"]) 
              (Pipe (Literal "bar") (Literal "baz"))) 
    $ parse "(foo bar) >> bar >> baz"
 

test_parse_func = do
  assertEqual (Func "join" [Literal "bar", Literal "baz"]) 
    $ parse "join bar baz"

  assertEqual (Func "join" [
      Literal "bar", 
      Literal "baz", 
      Func "split" [Literal "foo", Literal "qux"]]) 
    $ parse "join bar baz (split foo qux)"
 

test_parse_variable = do
  assertEqual (Group [Var "foo", Var "bar"]) 
    $ parse "$foo $bar"

  assertEqual (Group [Var "1", Var "2"]) 
    $ parse "$1 $2"

  assertEqual (Func "join" [Var "1", Var "bar", Var "2", Literal "foo"]) 
    $ parse "join $1 $bar $2 foo"


test_parse_quote = do
  assertEqual (Literal "join foo bar") 
    $ parse "'join foo bar'"
