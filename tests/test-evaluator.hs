{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Evaluator

main = htfMain htf_thisModulesTests

-- test_evaluate = do

test_eval = do
  assertEqual "foo" $ eval "foo" ""
  assertEqual "foo bar baz" $ eval "foo bar baz" ""
