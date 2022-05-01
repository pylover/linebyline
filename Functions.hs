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


funcArgs :: [String] -> [String] -> [String]
funcArgs ca [] = ca
funcArgs _ xs = xs


joinF :: Ctx -> [String] -> [String]
joinF c [] = joinF c [" "]
joinF (Ctx _ ca) (x:xs) = [intercalate x a]
  where a = funcArgs ca xs


splitF :: Ctx -> [String] -> [String]
splitF c [] = splitF c [" "]
splitF (Ctx _ ca) (x:xs) = split "" x a
  where a = spacer $ funcArgs ca xs
