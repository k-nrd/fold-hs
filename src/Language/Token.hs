module Language.Token where

newtype Token = Token (Int, String)
  deriving stock (Show, Eq)
