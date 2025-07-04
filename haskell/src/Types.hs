{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import qualified Crypto.Cipher.ChaChaPoly1305 as C
import Crypto.Error (CryptoFailable (..), throwCryptoError)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.MAC.Poly1305 (authTag)
import Crypto.Random (getRandomBytes)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)

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

-- | Produce the canonical byte encoding of a 'Type'.
canonicalBytes :: Type a -> ByteString
canonicalBytes t = B.pack (go t)
  where
    go :: Type b -> [Word8]
    go TInt = [0]
    go TString = [1]
    go TBool = [2]
    go (TPair a b) = 3 : go a ++ go b
    go (TList ty) = 4 : go ty

-- | Internal: derive a symmetric key from a 'Type'.
keyFromType :: Type a -> B.ByteString
keyFromType ty = convert (hash (canonicalBytes ty) :: Digest SHA256)

encrypt :: Type a -> ByteString -> IO ByteString
encrypt ty plaintext = do
  nonceBytes <- getRandomBytes 12
  let key = keyFromType ty
      nonce = case C.nonce12 nonceBytes of
        CryptoFailed _ -> error "invalid nonce"
        CryptoPassed n -> n
      st1 = throwCryptoError $ C.initialize key nonce
      st2 = C.finalizeAAD st1
      (out, st3) = C.encrypt plaintext st2
      tag = C.finalize st3
  pure $ nonceBytes `B.append` out `B.append` convert tag

-- | Decrypt if the value matches the expected type and authentication tag checks.
decrypt :: Type a -> Value -> ByteString -> Maybe ByteString
decrypt ty val input
  | not (matches val ty) = Nothing
  | B.length input < 28 = Nothing
  | otherwise =
      let (nonceBytes, rest) = B.splitAt 12 input
          (ct, tagBytes) = B.splitAt (B.length rest - 16) rest
          key = keyFromType ty
          nonce = case C.nonce12 nonceBytes of
            CryptoFailed _ -> error "invalid nonce"
            CryptoPassed n -> n
          st1 = throwCryptoError $ C.initialize key nonce
          st2 = C.finalizeAAD st1
          (pt, st3) = C.decrypt ct st2
          tag = C.finalize st3
       in case authTag tagBytes of
            CryptoFailed _ -> Nothing
            CryptoPassed t -> if t == tag then Just pt else Nothing
