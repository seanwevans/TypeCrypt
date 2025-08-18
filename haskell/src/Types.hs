{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import qualified Crypto.Cipher.ChaChaPoly1305 as C
import Crypto.Error (CryptoFailable (..), throwCryptoError)
import Crypto.Hash (SHA256)
import Crypto.KDF.HKDF (PRK, expand, extract)
import Crypto.MAC.Poly1305 (authTag)
import Crypto.Random (getRandomBytes)
import Data.ByteArray (constEq, convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
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

-- | Errors that can occur during decryption.
data DecryptError
  = TypeMismatch
  | TruncatedCiphertext
  | DecryptNonceError
  | DecryptCryptoError
  deriving (Eq, Show)

-- | Errors that can occur during encryption.
data EncryptError
  = NonceError
  | CryptoError
  deriving (Eq, Show)

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
keyFromType ty =
  let ikm = canonicalBytes ty
      salt = B8.pack "TypeCryptHKDFSalt"
      info = B8.pack "TypeCryptHKDFInfo"
      prk :: PRK SHA256
      prk = extract salt ikm
   in expand prk info 32

encrypt :: Type a -> ByteString -> IO (Either EncryptError ByteString)
encrypt ty plaintext = do
  nonceBytes <- getRandomBytes 12
  let key = keyFromType ty
  pure $ case C.nonce12 nonceBytes of
    CryptoFailed _ -> Left NonceError
    CryptoPassed nonce -> case C.initialize key nonce of
      CryptoFailed _ -> Left CryptoError
      CryptoPassed st1 ->
        let st2 = C.finalizeAAD st1
            (out, st3) = C.encrypt plaintext st2
            tag = C.finalize st3
         in Right $ nonceBytes `B.append` out `B.append` convert tag

-- | Decrypt if the value matches the expected type and authentication tag checks.
decrypt :: Type a -> Value -> ByteString -> Either DecryptError ByteString
decrypt ty val input
  | not (matches val ty) = Left TypeMismatch
  | B.length input < 28 = Left TruncatedCiphertext
  | otherwise =
      let (nonceBytes, rest) = B.splitAt 12 input
          (ct, tagBytes) = B.splitAt (B.length rest - 16) rest
          key = keyFromType ty
       in case C.nonce12 nonceBytes of
            CryptoFailed _ -> Left DecryptNonceError
            CryptoPassed nonce -> case C.initialize key nonce of
              CryptoFailed _ -> Left DecryptCryptoError
              CryptoPassed st1 ->
                let st2 = C.finalizeAAD st1
                    (pt, st3) = C.decrypt ct st2
                    tag = C.finalize st3
                 in case authTag tagBytes of
                      CryptoFailed _ -> Left DecryptCryptoError
                      CryptoPassed t ->
                        if constEq t tag then Right pt else Left DecryptCryptoError
