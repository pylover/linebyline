{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Evaluator

main = htfMain htf_thisModulesTests

test_eval = do
  assertEqual ["foo"] $ eval "print foo" []
  assertEqual ["foo"] $ eval "foo" []

