{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Parser

main = htfMain htf_thisModulesTests

test_show = do
  assertEqual "print foo bar" $ show (Func "print" [Value "foo", Value "bar"])
  assertEqual "foo + bar" $ show (Infix (Value "foo") "+" (Value "bar"))

-- test_matchParenthesis = do
--   assertEqual matchParenthesis 0 "" "foo bar (baz))"
-- test_parser = do
--   assertEqual (parse "print foo") (Func "print" ["foo"])
