module Helpers 
  ( split
  , spacer
  , quote
  , (+?)
  , splitByParenthesis
  , liftMaybe
  , readLine
  , exitError
  , exit
  )  where


import Data.List
import Data.String
import System.IO
import System.Exit
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe


splitPar :: Int -> [String] -> [String] -> ([String], [String])
splitPar _ left [] = (left, [])
splitPar i left ("(":xs) = splitPar (i + 1) (left ++ ["("]) xs
splitPar 0 left (")":xs) = (left, xs)
splitPar i left (")":xs) = splitPar (i - 1) (left ++ [")"]) xs
splitPar i left (x:xs) = splitPar i (left ++ [x]) xs


splitByParenthesis :: [String] -> ([String], [String])
splitByParenthesis = splitPar 0 []


spacer :: [String] -> String
spacer xs = intercalate " " xs


(+?) :: String -> [String] -> [String]
(+?) "" xs = xs
(+?) a xs = a : xs


quote :: String -> String
quote = decorate '\''


decorate :: Char -> String -> String
decorate c s  = c : s ++ [c]


findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)


split_ :: String -> String -> String -> [String]
split_ c n "" = [c]
split_ c n (s:ss)= case stripPrefix n (s:ss) of
  Just x -> c : split_ "" n x
  Nothing -> split_ (c ++ [s]) n ss


split :: String -> String -> [String]
split s xs = filter (/="") res
  where res = split_ "" s xs


liftMaybe :: Maybe a -> MaybeT IO a
liftMaybe = MaybeT . pure


readLine :: MaybeT IO String
readLine = do
  isClosed <- lift isEOF
  if isClosed 
    then mzero
    else lift getLine


exitError :: IO ()
exitError = exitWith (ExitFailure 84)


exit :: IO ()
exit = exitWith ExitSuccess
