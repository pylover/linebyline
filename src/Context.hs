module Context (Ctx(..)) where


data Ctx = Ctx { index :: Int, line :: String, args :: [String] }
  deriving Show
