module HackLine.Functions where

data Session = Session { lineNo :: Int, args :: [String]}

type Function = Session -> [String] -> [String]

functions :: [String]
functions = 
  [ "join"
  , "split"
  ]

getFunc :: String -> Function
getFunc "print" = funcPrint

funcPrint :: Function
funcPrint _ xs = xs
