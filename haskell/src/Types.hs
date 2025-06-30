module Types where

-- | Placeholder for type algebra definitions.
data Type
  = TInt
  | TString
  deriving (Show, Eq)

-- | Placeholder for value representations.
data Value
  = VInt Int
  | VString String
  deriving (Show, Eq)
