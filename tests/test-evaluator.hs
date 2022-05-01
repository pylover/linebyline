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
  assertEqual "foo" $ ev "foo" ""
  assertEqual "foo bar baz" $ ev "foo bar baz" ""
  assertEqual "join bar baz" $ ev "'join' bar baz" ""
  assertEqual "foo bar baz qux" $ ev "split ' ' :0 baz qux" "foo bar"


test_eval_pipe = do
  assertEqual "foo|bar" $ ev "split :: join '|'" "foo bar"
  assertEqual "foo|bar" $ ev "foo bar :: join '|'" ""
  assertEqual "foo|bar" $ ev "join '|' foo bar" "qux quux"
