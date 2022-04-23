{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Parser


main = htfMain htf_thisModulesTests


test_parse = do
  assertEqual Void $ parse ""
  assertEqual (Literal "foo") $ parse "foo"
  assertEqual (Group [Literal "foo", Literal "bar"]) $ parse "foo bar"
  assertEqual (Group [Literal "foo", Literal "bar", Literal "baz"]) 
    $ parse "foo (bar baz)"

  assertEqual (Pipe (Literal "foo") ">>|" (Literal "bar")) 
    $ parse "foo >>| bar"

  assertEqual (Pipe (Literal "foo") ">>|" 
              (Pipe (Literal "bar") ">>+" (Literal "baz"))) 
    $ parse "foo >>| bar >>+ baz"

  assertEqual (Pipe (Group [Literal "foo", Literal "bar"]) ">>|" 
              (Pipe (Literal "bar") ">>+" (Literal "baz"))) 
    $ parse "(foo bar) >>| bar >>+ baz"
  
  assertEqual (Func "join" [Literal "bar", Literal "baz"]) 
    $ parse "join bar baz"

  assertEqual (Func "join" [
      Literal "bar", 
      Literal "baz", 
      Func "split" [Literal "foo", Literal "qux"]]) 
    $ parse "join bar baz (split foo qux)"
