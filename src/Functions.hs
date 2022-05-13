module Functions 
  ( functions
  , getFunc
  , Signal (..)
  , CtxT (..)
  ) where


import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map, (!), member, fromList)
import Text.Regex.TDFA ((=~))
import Control.Monad.State (lift)
import Control.Monad.Trans.State (StateT, gets)

import Context
import Helpers (strReplace, capitalize, lower, upper)


data Signal = SuppressLine | SuppressAll
  deriving (Eq, Show)


type Function = [String] -> CtxT [String]
type CtxT a = StateT Ctx (Either Signal) a


functions :: Map String Function
functions = fromList
  [ ("join",        joinF       )
  , ("split",       splitF      )
  -- , ("grep",        grepF       )
  -- , ("grab",        grabF       )
  -- , ("break",       breakF      )
  -- , ("ignore",      ignoreF     )
  -- , ("replace",     replaceF    )
  -- , ("capitalize",  capitalizeF )
  -- , ("upper",       upperF      )
  -- , ("lower",       lowerF      )
  ]


getFunc :: String -> Function
getFunc = (functions ! )


funcArgs :: [String] -> CtxT [String]
funcArgs [] = gets args
funcArgs xs = return xs


joinF :: [String] -> CtxT [String]
joinF [] = joinF [" "]
joinF (x:xs) = funcArgs xs >>= return . (\a ->  [intercalate x a])


splitF :: [String] -> CtxT [String]
splitF [] = splitF [" "]
splitF (x:xs) = funcArgs xs >>= return . filter (/="") . splitOn x . unwords


-- grepF :: Ctx -> [String] -> Either Signal [String]
-- grepF c [] = grepF c [".*"]
-- grepF c (x:xs) = case filter (/="") m of
--   [] -> Left SuppressLine
--   _ -> Right a
--   where 
--     a = funcArgs (args c) xs
--     m = (=~x) <$> a
-- 
-- 
-- grabF :: Ctx -> [String] -> Either Signal [String]
-- grabF c [] = grabF c [".*"]
-- grabF c (x:xs) = case filter (/="") m of
--   [] -> Left SuppressLine
--   y -> Right y
--   where 
--     m = (=~x) <$> funcArgs (args c) xs
-- 
-- 
-- breakF :: Ctx -> [String] -> Either Signal [String]
-- breakF c [] = breakF c ["^$"]
-- breakF c (x:xs) = case any (=~x) a of
--   True -> Left SuppressAll
--   False -> Right a
--   where 
--     a = funcArgs (args c) xs
-- 
-- 
-- ignoreF :: Ctx -> [String] -> Either Signal [String]
-- ignoreF c [] = ignoreF c ["^$"]
-- ignoreF c (x:xs) = case any (=~x) a of
--   True -> Left SuppressLine
--   False -> Right a
--   where 
--     a = funcArgs (args c) xs
-- 
-- 
-- replaceF :: Ctx -> [String] -> Either Signal [String]
-- replaceF c (p:r:xs) = Right $ strReplace p r <$> funcArgs (args c) xs
-- replaceF c _ = Right (args c) 
-- 
-- 
-- capitalizeF :: Ctx -> [String] -> Either Signal [String]
-- capitalizeF c xs = Right $ capitalize <$> funcArgs (args c) xs
-- 
-- 
-- lowerF :: Ctx -> [String] -> Either Signal [String]
-- lowerF c xs = Right $ lower <$> funcArgs (args c) xs
-- 
-- 
-- upperF :: Ctx -> [String] -> Either Signal [String]
-- upperF c xs = Right $ upper <$> funcArgs (args c) xs
