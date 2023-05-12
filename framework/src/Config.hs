module Config where

data Tool = Tool {
  name :: String,
    execPath :: Maybe String,
    args :: [String]
  } deriving (Show, Eq)
