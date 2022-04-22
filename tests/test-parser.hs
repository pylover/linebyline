{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import HackLine.Parser

main = htfMain htf_thisModulesTests

test_tokenize_ = do
  assertEqual Void $ parse ""
