module HackLine.Functions where


import Data.List
import Text.Regex.TDFA

import HackLine.Context
import HackLine.Helpers


data Signal = IgnoreLine | SuppressLine | SuppressAll
  deriving (Eq, Show)


type Function = Ctx -> [String] -> Either Signal [String]


functions :: [String]
functions = 
  [ "join"
  , "split"
  , "grep"
  , "grab"
  ]


getFunc :: String -> Function
getFunc "join" = joinF
getFunc "split" = splitF
getFunc "grep" = grepF
getFunc "grab" = grabF
getFunc "" = joinF


funcArgs :: [String] -> [String] -> [String]
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


grepF :: Ctx -> [String] -> Either Signal [String]
grepF c [] = grepF c [".*"]
grepF c (x:xs) = case filter (\(_, y, _) -> y /= "") m of
  [] -> Left SuppressLine
  y -> Right $ asm <$> y
  where 
    m = (=~x) <$> funcArgs (args c) xs
    asm (b, z, a) = b ++ z ++ a


grabF :: Ctx -> [String] -> Either Signal [String]
grabF c [] = grabF c [".*"]
grabF c (x:xs) = case filter (/="") m of
  [] -> Left SuppressLine
  y -> Right y
  where 
    m = (=~x) <$> funcArgs (args c) xs
