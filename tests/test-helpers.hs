{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Helpers

main = htfMain htf_thisModulesTests

test_mPar = do
  assertEqual ("foo", "") (mPar 0 "foo" "")   
  assertEqual ("foo (bar) baz", "") (mPar 0 "" "foo (bar) baz)")
  assertEqual ("foo (bar) baz", " qux (quux)") 
    (mPar 0 "" "foo (bar) baz) qux (quux)")
