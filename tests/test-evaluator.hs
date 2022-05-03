{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Parser
import HackLine.Context
import HackLine.Evaluator


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
  assertEqual (Right "Foo Bar") $ ev "" "Foo Bar"
  assertEqual (Right "foo") $ ev "foo" ""
  assertEqual (Right "foo bar baz") $ ev "foo bar baz" ""
  assertEqual (Right "join bar baz") $ ev "'join' bar baz" ""
  assertEqual (Right "foo bar baz qux") $ ev "split ' ' :0 baz qux" "foo bar"


test_eval_pipe = do
  assertEqual (Right "foo|bar") $ ev "split :: join '|'" "foo bar"
  assertEqual (Right "foo|bar") $ ev "foo bar :: join '|'" ""
  assertEqual (Right "foo|bar") $ ev "join '|' foo bar" "qux quux"
  assertEqual (Right "foo,bar") $ ev "split :: join ',' :~" "foo bar"
