{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestFunctions (htf_thisModulesTests, main) where


import Test.Framework

import Functions
import Evaluator (eval)


main = htfMain htf_thisModulesTests


ev e a = eval e 0 a


test_func_split = do
  assertEqual (Right "foo bar baz") $ ev "split" "foo  bar baz"


test_func_search_and_replace = do
  assertEqual (Right "oof bar") 
    $ ev "replace foo oof" "foo bar"

  assertEqual (Right "oof oof bar") 
    $ ev "split :: replace foo oof" "foo foo bar"

  assertEqual (Right "foo bar") 
    $ ev "replace foo" "foo bar"

  assertEqual (Right "foo bar") 
    $ ev "replace" "foo bar"

  assertEqual (Right "") 
    $ ev "replace" ""


test_func_grep = do
  assertEqual (Right "a,1,2") 
    $ ev "split :: grep '[0-9]+' :: join ','" "a 1 2"

  assertEqual (Right "foo bar") 
    $ ev "grep 'foo'" "foo bar"

  assertEqual (Right "baz foo bar") 
    $ ev "grep 'foo'" "baz foo bar"

  assertEqual (Left SuppressLine) 
    $ ev "grep '1'" "baz foo bar"

  assertEqual (Left SuppressLine) 
    $ ev "ignore 'foo'" "baz foo bar"

  assertEqual (Left SuppressAll) 
    $ ev "break 'bar'" "baz foo bar"


test_func_igrep = do
  assertEqual (Right "A,1,2") 
    $ ev "split :: igrep '[a-z]+' :: join ','" "A 1 2"


test_func_grab = do
  assertEqual (Right "1,2") 
    $ ev "split :: grab '[0-9]+' :: join ','" "a 1 2"


test_func_capitalize = do
  assertEqual (Right "Foo bar") $ ev "capitalize" "foo bar"
  assertEqual (Right "Foo Bar") $ ev "split:: capitalize" "foo bar"


test_func_upper_lower = do
  assertEqual (Right "FOO BAR") $ ev "upper" "foo bar"
  assertEqual (Right "foo bar") $ ev "lower" "FOO BAR"
