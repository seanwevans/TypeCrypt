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
        ct <- encrypt TInt bs
        pure $ decrypt TInt (V TInt n) ct == Just bs
    it "roundtrips for String" $ property $ \(s :: String) bs ->
      ioProperty $ do
        ct <- encrypt TString bs
        pure $ decrypt TString (V TString s) ct == Just bs
    it "roundtrips for (Int, String)" $ property $ \(a :: Int) (b :: String) bs ->
      ioProperty $ do
        let ty = TPair TInt TString
            val = V ty (a, b)
        ct <- encrypt ty bs
        pure $ decrypt ty val ct == Just bs
    it "roundtrips for [Int]" $ property $ \(xs :: [Int]) bs ->
      ioProperty $ do
        let ty = TList TInt
            val = V ty xs
        ct <- encrypt ty bs
        pure $ decrypt ty val ct == Just bs
    it "roundtrips for nested (Int, [String])" $
      property $ \(n :: Int) (xs :: [String]) bs ->
        ioProperty $ do
          let ty = TPair TInt (TList TString)
              val = V ty (n, xs)
          ct <- encrypt ty bs
          pure $ decrypt ty val ct == Just bs
    it "matches Bool" $ property $ \b ->
      matches (V TBool b) TBool
    it "matches Pair" $ property $ \a b ->
      matches (V (TPair TInt TString) (a, b)) (TPair TInt TString)
    it "matches List" $ property $ \xs ->
      matches (V (TList TInt) xs) (TList TInt)
    it "roundtrips for Bool" $ property $ \(b :: Bool) bs ->
      ioProperty $ do
        ct <- encrypt TBool bs
        pure $ decrypt TBool (V TBool b) ct == Just bs
    it "roundtrips for Pair" $ property $ \(a :: Int) (b :: String) bs ->
      ioProperty $ do
        let t = TPair TInt TString
        ct <- encrypt t bs
        pure $ decrypt t (V t (a, b)) ct == Just bs
    it "roundtrips for List" $ property $ \(xs :: [Int]) bs ->
      ioProperty $ do
        let t = TList TInt
        ct <- encrypt t bs
        pure $ decrypt t (V t xs) ct == Just bs
    it "multiple encryptions use different nonces" $
      property $ \(bs :: ByteString) -> ioProperty $ do
        let ty = TInt
        ct1 <- encrypt ty bs
        ct2 <- encrypt ty bs
        pure (ct1 /= ct2)
    it "fails to decrypt with mismatched Value" $
      property $ \(bs :: ByteString) (s :: String) -> ioProperty $ do
        let ty = TInt
        ct <- encrypt ty bs
        pure $ decrypt ty (V TString s) ct == Nothing
    it "fails to decrypt with wrong Type" $
      property $ \(n :: Int) bs -> ioProperty $ do
        ct <- encrypt TInt bs
        pure $ decrypt TString (V TInt n) ct == Nothing

    it "fails to decrypt when ciphertext is truncated" $
      property $ \(n :: Int) (bs :: ByteString) -> ioProperty $ do
        ct <- encrypt TInt bs
        let truncated = B.take (B.length ct - 1) ct
        pure $ decrypt TInt (V TInt n) truncated == Nothing

    it "fails to decrypt when ciphertext is tampered" $
      property $ \(n :: Int) (bs :: ByteString) -> ioProperty $ do
        ct <- encrypt TInt bs
        let first = B.index ct 0
            tampered = B.cons (first `xor` 1) (B.drop 1 ct)
        pure $ decrypt TInt (V TInt n) tampered == Nothing

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
