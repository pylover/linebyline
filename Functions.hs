module HackLine.Functions where


import Data.List (intercalate)
import Data.Map (Map, (!), member, fromList)
import Text.Regex.TDFA ((=~))

import HackLine.Context
import HackLine.Helpers


data Signal = SuppressLine | SuppressAll
  deriving (Eq, Show)


type Function = Ctx -> [String] -> Either Signal [String]


functions :: Map String Function
functions = fromList
  [ ("join",  joinF)
  , ("split", splitF)
  , ("grep",  grepF)
  , ("grab",  grabF)
  , ("break", breakF)
  , ("ignore", ignoreF)
  ]


getFunc :: String -> Function
getFunc = (functions ! )


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


breakF :: Ctx -> [String] -> Either Signal [String]
breakF c [] = breakF c ["^$"]
breakF c (x:xs) = case any (=~x) a of
  True -> Left SuppressAll
  False -> Right a
  where 
    a = funcArgs (args c) xs


ignoreF :: Ctx -> [String] -> Either Signal [String]
ignoreF c [] = ignoreF c ["^$"]
ignoreF c (x:xs) = case any (=~x) a of
  True -> Left SuppressLine
  False -> Right a
  where 
    a = funcArgs (args c) xs
