module HackLine.Context where


data Ctx = Ctx { index :: Int, args :: [String] }
  deriving Show
