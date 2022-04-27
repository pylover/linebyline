module HackLine.Functions where


import Data.List

import HackLine.Context
import HackLine.Helpers


type Function = Ctx -> [String] -> [String]


functions :: [String]
functions = 
  [ "join"
  , "split"
  ]


getFunc :: String -> Function
getFunc "join" = joinF
getFunc "split" = splitF
getFunc "" = joinF


joinF :: Ctx -> [String] -> [String]
joinF c (x:xs) = [intercalate x xs]


splitF :: Ctx -> [String] -> [String]
splitF c (s:xs) = [spacer $ split "" s (spacer xs)]
