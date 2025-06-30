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

-- | Check whether a 'Value' conforms to a given 'Type'.
matches :: Value -> Type -> Bool
matches (VInt _)    TInt    = True
matches (VString _) TString = True
matches _           _       = False
