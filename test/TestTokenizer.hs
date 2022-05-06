{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestTokenizer (htf_thisModulesTests, main) where


import Test.Framework

import Tokenizer


main = htfMain htf_thisModulesTests


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
  assertEqual ["foo", ":::", "bar"] $ tokenize "foo ::: bar"
  assertEqual ["foo", "'join a b c'"] $ tokenize "foo 'join a b c'"
  assertEqual ["foo", ":1:9"] $ tokenize "foo :1:9"

  -- Newline
  assertEqual ["foo", "\n", "bar"] $ tokenize "foo \n bar"


test_tokenize_escape = do
  assertEqual ["foo", "\\:", "bar"] $ tokenize "foo \\: bar"
  assertEqual ["foo", "\\:0", "bar"] $ tokenize "foo \\:0 bar"
  assertEqual ["\\:"] $ tokenize "\\:"
  assertEqual ["\\::"] $ tokenize "\\::"
  assertEqual ["\\:\\:"] $ tokenize "\\:\\:"
  assertEqual ["foo\\:\\:baz"] $ tokenize "foo\\:\\:baz"
