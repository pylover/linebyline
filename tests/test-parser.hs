{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Tokenizer
import HackLine.Parser
import HackLine.Helpers

main = htfMain htf_thisModulesTests

test_dump = do
  assertEqual "(print foo bar)"
    $ dump (Func "print" [Literal "foo", Literal "bar"])
  assertEqual "(foo + bar)" 
    $ dump (Infix (Literal "foo") "+" (Literal "bar"))

test_parse = do
  assertEqual (Func "print" [Literal "foo"])
    $ parse (tokenize "" "print foo") 

  assertEqual "(print foo)"
    $ dump (parse (tokenize "" "print foo"))

  assertEqual (Func "print" 
      [ Literal "foo"
      , Literal "2"
      , Literal "+"
      , Literal "3"])
    $ parse (tokenize "" "print foo 2+3")

  assertEqual (Func "print" 
      [ Literal "foo"
      , Infix (Literal "2") "+" (Literal "3")])    
    $ parse (tokenize "" "print foo (2+3)")

  assertEqual (Func "print" 
      [ Literal "foo"
      , Infix (Literal "2") "+" (Literal "3")])    
    $ parse (tokenize "" "print foo (2+3+(4+1))")


-- 2 + 3 + 4 + 5
