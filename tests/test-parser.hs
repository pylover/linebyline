{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Parser


main = htfMain htf_thisModulesTests


test_tokenize_ = do
  assertEqual Void $ parse ""
  assertEqual (Literal "foo") $ parse "foo"
  assertEqual (Group [Literal "foo", Literal "bar"]) $ parse "foo bar"
  assertEqual (Group [Literal "foo", Group [Literal "bar", Literal "baz"]]) 
    $ parse "foo (bar baz)"

  assertEqual (Pipe (Literal "foo") ">>|" (Literal "bar")) 
    $ parse "foo >>| bar"

  assertEqual (Pipe (Literal "foo") ">>|" 
              (Pipe (Literal "bar") ">>+" (Literal "baz"))) 
    $ parse "foo >>| bar >>+ baz"
