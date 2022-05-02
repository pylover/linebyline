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
  assertEqual (Right "") $ ev "" ""
  assertEqual (Right "Foo Bar") $ ev "" "Foo Bar"
  assertEqual (Right "foo") $ ev "foo" ""
  assertEqual (Right "foo bar baz") $ ev "foo bar baz" ""
  assertEqual (Right "join bar baz") $ ev "'join' bar baz" ""
  assertEqual (Right "foo bar baz qux") $ ev "split ' ' :0 baz qux" "foo bar"


test_eval_pipe = do
  assertEqual (Right "foo|bar") $ ev "split :: join '|'" "foo bar"
  assertEqual (Right "foo|bar") $ ev "foo bar :: join '|'" ""
  assertEqual (Right "foo|bar") $ ev "join '|' foo bar" "qux quux"
