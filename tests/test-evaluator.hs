{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Parser
import HackLine.Context
import HackLine.Evaluator


main = htfMain htf_thisModulesTests


test_evaluate = do
  assertEqual ["foo bar baz"] 
    $ evaluate (Ctx 0 []) (parse "join ' ' foo bar baz")

  assertEqual ["foo,bar,baz"] 
    $ evaluate (Ctx 0 []) (parse "join , foo bar baz")

test_eval = do
  assertEqual "foo" $ eval "foo" ""
  assertEqual "foo bar baz" $ eval "foo bar baz" ""
  assertEqual "join bar baz" $ eval "'join' bar baz" ""
  
