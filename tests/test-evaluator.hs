{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Parser
import HackLine.Context
import HackLine.Evaluator


main = htfMain htf_thisModulesTests

ev e a = eval e 0 a

test_evaluate_literal = do
  assertEqual ["foo bar baz"] 
    $ evaluate (Ctx 0 []) (parse "join ' ' foo bar baz")

  assertEqual ["foo,bar,baz"] 
    $ evaluate (Ctx 0 []) (parse "join ',' foo bar baz")

  assertEqual ["foo,bar,baz"] 
    $ evaluate (Ctx 0 []) (parse "join , foo bar baz")

  assertEqual ["foo::bar::baz"] 
    $ evaluate (Ctx 0 []) (parse "join '::' foo bar baz")

  assertEqual ["foo:bar:baz"] 
    $ evaluate (Ctx 0 []) (parse "join : foo bar baz")


test_evaluate_var = do
  assertEqual ["foo", "bar", "baz"] 
    $ evaluate (Ctx 0 ["foo", "bar"]) (parse ":0 :1 baz")

  assertEqual ["foo", "bar", ":2"] 
    $ evaluate (Ctx 0 ["foo", "bar"]) (parse ":0 :1 :2")

  assertEqual ["foo", "bar", ":2"] 
    $ evaluate (Ctx 0 ["foo", "bar"]) (parse ":0 :1 :2")

  assertEqual ["foo", "bar", ":baz"] 
    $ evaluate (Ctx 0 ["foo", "bar"]) (parse ":0 :1 :baz")


test_eval = do
  assertEqual (Just "") $ ev "" ""
  assertEqual (Just "Foo Bar") $ ev "" "Foo Bar"
  assertEqual (Just "foo") $ ev "foo" ""
  assertEqual (Just "foo bar baz") $ ev "foo bar baz" ""
  assertEqual (Just "join bar baz") $ ev "'join' bar baz" ""
  assertEqual (Just "foo bar baz qux") $ ev "split ' ' :0 baz qux" "foo bar"


test_eval_pipe = do
  assertEqual (Just "foo|bar") $ ev "split :: join '|'" "foo bar"
  assertEqual (Just "foo|bar") $ ev "foo bar :: join '|'" ""
  assertEqual (Just "foo|bar") $ ev "join '|' foo bar" "qux quux"
