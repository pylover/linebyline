{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestEvaluator (htf_thisModulesTests, main) where


import Test.Framework

import Parser
import Context
import Evaluator


main = htfMain htf_thisModulesTests


ev e a = eval e 0 a


test_evaluate_literal = do
  assertEqual (Right ["foo bar baz"])
    $ evaluate (Ctx 0 "" []) (parse "join ' ' foo bar baz")

  assertEqual (Right ["foo,bar,baz"])
    $ evaluate (Ctx 0 "" []) (parse "join ',' foo bar baz")

  assertEqual (Right ["foo,bar,baz"]) 
    $ evaluate (Ctx 0 "" []) (parse "join , foo bar baz")

  assertEqual (Right ["foo::bar::baz"]) 
    $ evaluate (Ctx 0 "" []) (parse "join '::' foo bar baz")

  assertEqual (Right ["foo:bar:baz"]) 
    $ evaluate (Ctx 0 "" []) (parse "join : foo bar baz")


test_evaluate_var = do
  assertEqual (Right ["foo", "bar", "baz"]) 
    $ evaluate (Ctx 0 "" ["foo", "bar"]) (parse ":0 :1 baz")

  assertEqual (Right ["foo", "bar", ":2"]) 
    $ evaluate (Ctx 0 "" ["foo", "bar"]) (parse ":0 :1 :2")

  assertEqual (Right ["foo", "bar", ":2"]) 
    $ evaluate (Ctx 0 "" ["foo", "bar"]) (parse ":0 :1 :2")

  assertEqual (Right ["foo", "bar", ":baz"]) 
    $ evaluate (Ctx 0 "" ["foo", "bar"]) (parse ":0 :1 :baz")

  assertEqual (Right ["a", "b", "c", "d"]) 
    $ evaluate (Ctx 0 "" ["a", "b", "c", "d"]) (parse ":0~")

  assertEqual (Right ["a", "b", "c"]) 
    $ evaluate (Ctx 0 "" ["a", "b", "c", "d"]) (parse ":0~2")

  assertEqual (Right ["a", "b", "c"]) 
    $ evaluate (Ctx 0 "" ["a", "b", "c", "d"]) (parse ":~2")

  assertEqual (Right ["a", "b", "c", "d"]) 
    $ evaluate (Ctx 0 "" ["a", "b", "c", "d"]) (parse ":~")

  assertEqual (Right ["2"]) 
    $ evaluate (Ctx 2 "" []) (parse ":n")

  assertEqual (Right [""]) 
    $ evaluate (Ctx 2 "" ["foo bar"]) (parse ":l")

  assertEqual (Right ["baz qux"]) 
    $ evaluate (Ctx 2 "baz qux" ["foo bar"]) (parse ":l")


test_eval = do
  assertEqual (Right "") $ ev "" ""
  assertEqual (Right "") $ ev "" "Foo Bar"
  assertEqual (Right "foo") $ ev "foo" ""
  assertEqual (Right "foo bar baz") $ ev "foo bar baz" ""
  assertEqual (Right "join bar baz") $ ev "'join' bar baz" ""
  assertEqual (Right "foo bar baz qux") $ ev "split ' ' :0 baz qux" "foo bar"
  assertEqual (Right "\n") $ ev "\n" ""


test_eval_pipe = do
  assertEqual (Right "foo|bar") $ ev "split :: join '|'" "foo bar"
  assertEqual (Right "foo|bar") $ ev "foo bar :: join '|'" ""
  assertEqual (Right "foo|bar") $ ev "join '|' foo bar" "qux quux"
  assertEqual (Right "foo,bar") $ ev "split :: join ',' :~" "foo bar"
  assertEqual (Right "foo|bar,foo,bar") 
    $ ev "split :: join ',' (join '|' :~) :~" "foo bar"


test_eval_grep = do
  assertEqual (Right "1,2") $ ev "split :: grep '[0-9]+' :: join ','" "a 1 2"
  assertEqual (Right "foo bar") $ ev "grep 'foo'" "foo bar"
  assertEqual (Right "baz foo bar") $ ev "grep 'foo'" "baz foo bar"
  assertEqual (Left SuppressLine) $ ev "grep '1'" "baz foo bar"
  assertEqual (Left SuppressLine) $ ev "ignore 'foo'" "baz foo bar"
  assertEqual (Left SuppressAll) $ ev "break 'bar'" "baz foo bar"


test_eval_after = do
  assertEqual (Right "a,b a|b") 
    $ ev "split :: join ',' ::: split :: join '|'" "a b"


test_search_and_replace = do
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
