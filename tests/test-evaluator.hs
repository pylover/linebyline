{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Parser
import HackLine.Context
import HackLine.Evaluator


main = htfMain htf_thisModulesTests

ev e a = eval e 0 a

test_evaluate_literal = do
  assertEqual (Right "foo bar baz")
    $ eval_ (Ctx 0 "" []) [] (parse "join ' ' foo bar baz")

  assertEqual (Right "foo,bar,baz")
    $ eval_ (Ctx 0 "" []) [] (parse "join ',' foo bar baz")

  assertEqual (Right "foo,bar,baz") 
    $ eval_ (Ctx 0 "" []) [] (parse "join , foo bar baz")

  assertEqual (Right "foo::bar::baz") 
    $ eval_ (Ctx 0 "" []) [] (parse "join '::' foo bar baz")

  assertEqual (Right "foo:bar:baz") 
    $ eval_ (Ctx 0 "" []) [] (parse "join : foo bar baz")


test_evaluate_var = do
  assertEqual (Right "foo bar baz") 
    $ eval_ (Ctx 0 "" ["foo", "bar"]) [] (parse ":0 :1 baz")

  assertEqual (Right "foo bar :2") 
    $ eval_ (Ctx 0 "" ["foo", "bar"]) [] (parse ":0 :1 :2")

  assertEqual (Right "foo bar :2") 
    $ eval_ (Ctx 0 "" ["foo", "bar"]) [] (parse ":0 :1 :2")

  assertEqual (Right "foo bar :baz") 
    $ eval_ (Ctx 0 "" ["foo", "bar"]) [] (parse ":0 :1 :baz")

  assertEqual (Right "a b c d") 
    $ eval_ (Ctx 0 "" ["a", "b", "c", "d"]) [] (parse ":0~")

  assertEqual (Right "a b c") 
    $ eval_ (Ctx 0 "" ["a", "b", "c", "d"]) [] (parse ":0~2")

  assertEqual (Right "a b c") 
    $ eval_ (Ctx 0 "" ["a", "b", "c", "d"]) [] (parse ":~2")

  assertEqual (Right "a b c d") 
    $ eval_ (Ctx 0 "" ["a", "b", "c", "d"]) [] (parse ":~")

  assertEqual (Right "2") 
    $ eval_ (Ctx 2 "" []) [] (parse ":n")

  assertEqual (Right "") 
    $ eval_ (Ctx 2 "" ["foo bar"]) [] (parse ":l")

  assertEqual (Right "baz qux") 
    $ eval_ (Ctx 2 "baz qux" ["foo bar"]) [] (parse ":l")


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
