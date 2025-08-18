{-# LANGUAGE ScopedTypeVariables #-}

import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Test.Hspec
import Test.QuickCheck
import Types

instance Arbitrary ByteString where
  arbitrary = B.pack <$> arbitrary

main :: IO ()
main = hspec $ do
  describe "matches" $ do
    it "returns True for matching Type and Value" $ property $ \n ->
      matches (V TInt n) TInt
    it "returns False for mismatched Type" $ property $ \n ->
      not (matches (V TInt n) TString)
    it "matches nested Pair/List" $ property $ \n xs ->
      matches
        (V (TPair TInt (TList TString)) (n, xs))
        (TPair TInt (TList TString))

  describe "encrypt/decrypt" $ do
    it "roundtrips for Int" $ property $ \(n :: Int) bs ->
      ioProperty $ do
        ctE <- encrypt TInt bs
        pure $ either (const False) (\ct -> decrypt TInt (V TInt n) ct == Right bs) ctE
    it "roundtrips for String" $ property $ \(s :: String) bs ->
      ioProperty $ do
        ctE <- encrypt TString bs
        pure $ either (const False) (\ct -> decrypt TString (V TString s) ct == Right bs) ctE
    it "roundtrips for (Int, String)" $ property $ \(a :: Int) (b :: String) bs ->
      ioProperty $ do
        let ty = TPair TInt TString
            val = V ty (a, b)
        ctE <- encrypt ty bs
        pure $ either (const False) (\ct -> decrypt ty val ct == Right bs) ctE
    it "roundtrips for [Int]" $ property $ \(xs :: [Int]) bs ->
      ioProperty $ do
        let ty = TList TInt
            val = V ty xs
        ctE <- encrypt ty bs
        pure $ either (const False) (\ct -> decrypt ty val ct == Right bs) ctE
    it "roundtrips for nested (Int, [String])" $
      property $ \(n :: Int) (xs :: [String]) bs ->
        ioProperty $ do
          let ty = TPair TInt (TList TString)
              val = V ty (n, xs)
          ctE <- encrypt ty bs
          pure $ either (const False) (\ct -> decrypt ty val ct == Right bs) ctE
    it "matches Bool" $ property $ \b ->
      matches (V TBool b) TBool
    it "matches Pair" $ property $ \a b ->
      matches (V (TPair TInt TString) (a, b)) (TPair TInt TString)
    it "matches List" $ property $ \xs ->
      matches (V (TList TInt) xs) (TList TInt)
    it "roundtrips for Bool" $ property $ \(b :: Bool) bs ->
      ioProperty $ do
        ctE <- encrypt TBool bs
        pure $ either (const False) (\ct -> decrypt TBool (V TBool b) ct == Right bs) ctE
    it "roundtrips for Pair" $ property $ \(a :: Int) (b :: String) bs ->
      ioProperty $ do
        let t = TPair TInt TString
        ctE <- encrypt t bs
        pure $ either (const False) (\ct -> decrypt t (V t (a, b)) ct == Right bs) ctE
    it "roundtrips for List" $ property $ \(xs :: [Int]) bs ->
      ioProperty $ do
        let t = TList TInt
        ctE <- encrypt t bs
        pure $ either (const False) (\ct -> decrypt t (V t xs) ct == Right bs) ctE
    it "multiple encryptions use different nonces" $
      property $ \(bs :: ByteString) -> ioProperty $ do
        let ty = TInt
        ct1E <- encrypt ty bs
        ct2E <- encrypt ty bs
        pure $ case (ct1E, ct2E) of
          (Right ct1, Right ct2) -> ct1 /= ct2
          _ -> False
    it "fails to decrypt with mismatched Value" $
      property $ \(bs :: ByteString) (s :: String) -> ioProperty $ do
        let ty = TInt
        ctE <- encrypt ty bs
        pure $ either (const False) (\ct -> decrypt ty (V TString s) ct == Left TypeMismatch) ctE
    it "fails to decrypt with wrong Type" $
      property $ \(n :: Int) bs -> ioProperty $ do
        ctE <- encrypt TInt bs
        pure $ either (const False) (\ct -> decrypt TString (V TInt n) ct == Left TypeMismatch) ctE

    it "fails to decrypt when ciphertext is truncated" $
      property $ \(n :: Int) (bs :: ByteString) -> ioProperty $ do
        ctE <- encrypt TInt bs
        pure $ case ctE of
          Right ct ->
            let truncated = B.take (B.length ct - 1) ct
             in case decrypt TInt (V TInt n) truncated of
                  Left TruncatedCiphertext -> True
                  Left DecryptCryptoError -> True
                  _ -> False
          Left _ -> False

    it "fails to decrypt when ciphertext is tampered" $
      property $ \(n :: Int) (bs :: ByteString) -> ioProperty $ do
        ctE <- encrypt TInt bs
        pure $ case ctE of
          Right ct ->
            let first = B.index ct 0
                tampered = B.cons (first `xor` 1) (B.drop 1 ct)
             in decrypt TInt (V TInt n) tampered == Left DecryptCryptoError
          Left _ -> False

  describe "key derivation" $ do
    it "key derivation is deterministic" $ do
      let ty = TList TInt
          k1 = keyFromType ty
          k2 = keyFromType ty
      k1 `shouldBe` k2
    it "different types yield different keys" $ do
      let k1 = keyFromType TInt
          k2 = keyFromType TString
      k1 `shouldNotBe` k2
    it "derive_key_deterministic" $
      property $
        let ty = TList TInt
         in keyFromType ty == keyFromType ty
    it "derive_key_distinct" $
      property $
        keyFromType TInt /= keyFromType TString
