module Language.Token where

data Token = Token
  { line :: Int,
    literal :: String
  }
  deriving stock (Show, Eq)
