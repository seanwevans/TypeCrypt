{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import qualified Crypto.Cipher.ChaChaPoly1305 as C
import Crypto.Error (CryptoFailable (..), throwCryptoError)
import Crypto.MAC.Poly1305 (authTag)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

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

-- | Internal: derive a symmetric key from a 'Type'.
keyFromType :: Type a -> B.ByteString
keyFromType TInt = B.replicate 32 0
keyFromType TString = B.replicate 32 1
keyFromType TBool = B.replicate 32 2
keyFromType (TPair _ _) = B.replicate 32 3
keyFromType (TList _) = B.replicate 32 4

-- | Encrypt the input using ChaCha20-Poly1305 with a key derived from the type.
encrypt :: Type a -> ByteString -> ByteString
encrypt ty plaintext =
  let key = keyFromType ty
      nonce = either (error "invalid nonce") id $ C.nonce12 (B.replicate 12 0)
      st1 = throwCryptoError $ C.initialize key nonce
      st2 = C.finalizeAAD st1
      (out, st3) = C.encrypt plaintext st2
      tag = C.finalize st3
   in out `B.append` convert tag

-- | Decrypt if the value matches the expected type and authentication tag checks.
decrypt :: Type a -> Value -> ByteString -> Maybe ByteString

decrypt ty val input
  | not (matches val ty) = Nothing
  | B.length input < 16 = Nothing
  | otherwise =
      let (ct, tagBytes) = B.splitAt (B.length input - 16) input
          key = keyFromType ty
          nonce = either (error "invalid nonce") id $ C.nonce12 (B.replicate 12 0)
          st1 = throwCryptoError $ C.initialize key nonce
          st2 = C.finalizeAAD st1
          (pt, st3) = C.decrypt ct st2
          tag = C.finalize st3
       in case authTag tagBytes of
            CryptoFailed _ -> Nothing
            CryptoPassed t -> if t == tag then Just pt else Nothing
