{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestEvaluator (htf_thisModulesTests, main) where


import Test.Framework

import Parser
import Context
import Evaluator
import Control.Monad.Trans.State (evalStateT)


main = htfMain htf_thisModulesTests


ev e a = eval e 0 a
evz e = eval e 0 ""

evc e c = evalStateT (evaluate (parse e)) c


test_eval_literal = do
  assertEqual (Right "foo bar baz") $ evz "join ' ' foo bar baz"
  assertEqual (Right "foo,bar,baz") $ evz "join ',' foo bar baz"
  assertEqual (Right "foo,bar,baz") $ evz "join , foo bar baz"
  assertEqual (Right "foo::bar::baz") $ evz "join '::' foo bar baz"
  assertEqual (Right "foo:bar:baz") $ evz "join ':' foo bar baz"


test_evaluate_var = do
  assertEqual (Right ["foo", "bar", "baz"]) 
    $ evc ":0 :1 baz" (Ctx 0 "" ["foo", "bar"]) 

  assertEqual (Right ["foo", "bar", ":2"]) 
    $ evc ":0 :1 :2" (Ctx 0 "" ["foo", "bar"]) 

  assertEqual (Right ["foo", "bar", ":2"]) 
    $ evc ":0 :1 :2" (Ctx 0 "" ["foo", "bar"]) 

  assertEqual (Right ["foo", "bar", ":baz"]) 
    $ evc ":0 :1 :baz" (Ctx 0 "" ["foo", "bar"]) 

  assertEqual (Right ["a", "b", "c", "d"]) 
    $ evc ":0~" (Ctx 0 "" ["a", "b", "c", "d"]) 

  assertEqual (Right ["a", "b", "c"]) 
    $ evc ":0~2" (Ctx 0 "" ["a", "b", "c", "d"]) 

  assertEqual (Right ["a", "b", "c"]) 
    $ evc ":~2" (Ctx 0 "" ["a", "b", "c", "d"]) 

  assertEqual (Right ["a", "b", "c", "d"]) 
    $ evc ":~" (Ctx 0 "" ["a", "b", "c", "d"]) 

  assertEqual (Right ["2"]) 
    $ evc ":n" (Ctx 2 "" []) 

  assertEqual (Right [""]) 
    $ evc ":l" (Ctx 2 "" ["foo bar"]) 

  assertEqual (Right ["baz qux"]) 
    $ evc ":l" (Ctx 2 "baz qux" ["foo bar"]) 


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


test_eval_after = do
  assertEqual (Right "a,b a|b") 
    $ ev "split :: join ',' ::: split :: join '|'" "a b"
