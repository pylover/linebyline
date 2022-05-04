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

  -- Pipe related
  assertEqual ["foo", "::", "bar"] $ tokenize "foo:: bar"
  assertEqual ["foo", "::", "bar"] $ tokenize "foo :: bar"
  assertEqual ["foo", "::", "bar"] $ tokenize "foo::bar"
  assertEqual ["foo", "'join a b c'"] $ tokenize "foo 'join a b c'"
  assertEqual ["foo", ":1:9"] $ tokenize "foo :1:9"
  
  -- Newline
  assertEqual ["foo", "\n", "bar"] $ tokenize "foo \n bar"
