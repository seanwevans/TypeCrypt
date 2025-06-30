{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import Data.ByteString (ByteString)

-- | Type algebra using GADTs.
-- Each constructor carries the concrete type it represents.
data Type a where
  TInt :: Type Int
  TString :: Type String
  TBool :: Type Bool
  TPair :: Type a -> Type b -> Type (a, b)
  TList :: Type a -> Type [a]

-- Show instance for existential 'Type'
deriving instance Show (Type a)

-- | Existential wrapper for values.
-- A 'Value' pairs a runtime Haskell value with a witness of its type.
data Value where
  V :: Type a -> a -> Value

-- Show instance for 'Value'
instance Show Value where
  show (V TInt n) = "VInt " ++ show n
  show (V TString s) = "VString " ++ show s
  show (V TBool b) = "VBool " ++ show b
  show (V (TPair _ _) _) = "VPair"
  show (V (TList _) _) = "VList"

-- | Check if a 'Value' matches a 'Type'.
-- Returns 'True' when the internal type witness equals the given type.
matches :: Value -> Type a -> Bool
matches (V t _) t' = sameType t t'

-- | Compare two type witnesses for equality.
sameType :: Type a -> Type b -> Bool
sameType TInt TInt = True
sameType TString TString = True
sameType TBool TBool = True
sameType (TPair a1 b1) (TPair a2 b2) = sameType a1 a2 && sameType b1 b2
sameType (TList t1) (TList t2) = sameType t1 t2
sameType _ _ = False

-- | Placeholder encryption using types as keys.
-- For now this simply echoes the input.
encrypt :: Type a -> ByteString -> ByteString
encrypt _ bs = bs

-- | Decrypt only if the provided value matches the type key.
decrypt :: Type a -> Value -> ByteString -> Maybe ByteString
decrypt t v bs
  | matches v t = Just bs
  | otherwise = Nothing
