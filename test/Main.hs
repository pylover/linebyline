{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main ( main ) where


import Test.Framework

import {-@ HTF_TESTS @-} TestHelpers
import {-@ HTF_TESTS @-} TestTokenizer
import {-@ HTF_TESTS @-} TestParser
import {-@ HTF_TESTS @-} TestEvaluator


main :: IO ()
main = htfMain htf_importedTests 
