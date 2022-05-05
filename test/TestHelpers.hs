{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestHelpers (htf_thisModulesTests) where


import Test.Framework

import Helpers


main = htfMain htf_thisModulesTests


test_split = do
  assertEqual ["foo", "bar", "baz"] $ split " " "foo bar baz"
  assertEqual ["foo", "bar", "baz"] $ split "," "foo,bar,baz"
  assertEqual ["foo", "bar", "baz"] $ split "," ",foo,bar,baz,"
  assertEqual ["foo", "bar"] $ split ",," ",,foo,,bar,,"
  assertEqual ["foo", "bar", "baz"] $ split " " "foo   bar baz"
