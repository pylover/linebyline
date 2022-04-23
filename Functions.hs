module HackLine.Functions where


import Data.List

import HackLine.Context


type Function = Ctx -> [String] -> [String]


functions :: [String]
functions = 
  [ "join"
  , "split"
  ]


getFunc :: String -> Function
getFunc "join" = joinF


joinF :: Ctx -> [String] -> [String]
joinF c (x:xs) = [intercalate x xs]
