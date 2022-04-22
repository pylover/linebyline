{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Evaluator

main = htfMain htf_thisModulesTests

test_eval = do
  assertEqual ["foo"] $ eval "print foo" []
  assertEqual ["foo"] $ eval "foo" []
  assertEqual ["foo", "bar", "baz"] $ eval "foo (bar) baz" []
  assertEqual ["foo", "bar", "baz", "qux"] $ eval "foo (bar baz) qux" []
  assertEqual ["foo", "bar", "baz", "qux", "quux", "thud"] 
    $ eval "foo (bar baz (qux quux) thud)" []
