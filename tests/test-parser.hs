{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Tokenizer
import HackLine.Parser
import HackLine.Helpers

main = htfMain htf_thisModulesTests

test_dump = do
  assertEqual "print foo bar"
    $ dump (Func "print" [Literal "foo", Literal "bar"])
  assertEqual "foo + bar" 
    $ dump (Infix (Literal "foo") "+" (Literal "bar"))

-- test_parse = do
--   assertEqual (Func "print" "foo" :| []) 
--     $ parse <$> (tokenize "" "print foo") 
