module HackLine.Functions where


import Data.List

import HackLine.Context
import HackLine.Helpers


data Signal = SuppressLine | SuppressAll
  deriving (Eq, Show)


type Function = Ctx -> [String] -> Either Signal [String]


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
-- funcArgs ca xs = ca ++ xs
-- Uncomment to ignore positional args when context has arguments
funcArgs ca [] = ca
funcArgs _ xs = xs


joinF :: Ctx -> [String] -> Either Signal [String]
joinF c [] = joinF c [" "]
joinF c (x:xs) = Right $ [intercalate x a]
  where a = funcArgs (args c) xs


splitF :: Ctx -> [String] -> Either Signal [String]
splitF c [] = splitF c [" "]
splitF c (x:xs) = Right $ split x a
  where a = spacer $ funcArgs (args c) xs
