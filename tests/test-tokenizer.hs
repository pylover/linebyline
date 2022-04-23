{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Tokenizer

main = htfMain htf_thisModulesTests

test_tokenize_ = do
  assertEqual ["foo"] $ tokenize_ "foo" ""
  
test_tokenize = do
  assertEqual [] $ tokenize ""
  assertEqual ["foo", "bar"] $ tokenize "foo bar"
  assertEqual ["foo", "(", "bar", ")"] $ tokenize "foo (bar)"
  assertEqual ["foo", "(", "bar", "(", "baz", ")", ")"] 
    $ tokenize "foo (bar (baz))"

  assertEqual ["foo", ">>", "bar"] $ tokenize "foo >> bar"
  assertEqual ["foo", ">>", "bar"] $ tokenize "foo>>bar"
