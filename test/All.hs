{-# OPTIONS_GHC -F -pgmF htfpp #-}
module All ( main ) where


import Test.Framework

import {-@ HTF_TESTS @-} TestFunctions  hiding (main)
import {-@ HTF_TESTS @-} TestTokenizer  hiding (main)
import {-@ HTF_TESTS @-} TestParser     hiding (main)
import {-@ HTF_TESTS @-} TestEvaluator  hiding (main)


main :: IO ()
main = htfMain htf_importedTests 
