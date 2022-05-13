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
  , ("grep",        grepF       )
  , ("grab",        grabF       )
  , ("break",       breakF      )
  , ("ignore",      ignoreF     )
  , ("replace",     replaceF    )
  , ("capitalize",  capitalizeF )
  , ("upper",       upperF      )
  , ("lower",       lowerF      )
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


grepF :: [String] -> CtxT [String]
grepF [] = grepF [".*"]
grepF (x:xs) = do
  a <- funcArgs xs 
  case filter (/="") ((=~x) <$> a) of
    [] -> lift $ Left SuppressLine
    _ -> return a
  

grabF :: [String] -> CtxT [String]
grabF [] = grabF [".*"]
grabF (x:xs) = do
  a <- funcArgs xs 
  case filter (/="") ((=~x) <$> a) of
    [] -> lift $ Left SuppressLine
    y -> return y


breakF :: [String] -> CtxT [String]
breakF [] = breakF ["^$"]
breakF (x:xs) = do
  a <- funcArgs xs
  case any (=~x) a of
    True -> lift $ Left SuppressAll
    False -> return a


ignoreF :: [String] -> CtxT [String]
ignoreF [] = ignoreF ["^$"]
ignoreF (x:xs) = do
  a <- funcArgs xs
  case any (=~x) a of
    True -> lift $ Left SuppressLine
    False -> return a


replaceF :: [String] -> CtxT [String]
replaceF (p:r:xs) = funcArgs xs >>= return . fmap (strReplace p r)
replaceF _ = gets args


capitalizeF :: [String] -> CtxT [String]
capitalizeF xs = funcArgs xs >>= return . fmap capitalize


lowerF :: [String] -> CtxT [String]
lowerF xs = funcArgs xs >>= return . fmap lower


upperF :: [String] -> CtxT [String]
upperF xs = funcArgs xs >>= return . fmap upper
