{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Tokenizer
import HackLine.Parser
import HackLine.Helpers

main = htfMain htf_thisModulesTests

test_dump = do
  assertEqual "(print foo bar)"
    $ dump (Func "print" [Literal "foo", Literal "bar"])

  assertEqual "(print foo)"
    $ dump (parse "print foo")

test_parse = do
  assertEqual (Func "print" [Literal "foo"])
    $ parse "print foo"

  assertEqual (Func "print" [Literal "foo", Literal "2+3"])
    $ parse "print foo 2+3"

  assertEqual (Func "print" [Literal "foo", Literal "2+3"])
    $ parse "print foo (2+3)"

  assertEqual (Func "print" 
      [ Literal "foo"
      , Func "print" [Literal "2", Literal "+", Literal "3"]])
    $ parse "print foo (2 + 3)"
