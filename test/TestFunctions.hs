{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestFunctions (htf_thisModulesTests, main) where


import Test.Framework

import Functions


main = htfMain htf_thisModulesTests
