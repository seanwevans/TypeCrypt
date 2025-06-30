{-# LANGUAGE ScopedTypeVariables #-}

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
      let ct = encrypt TInt bs
       in decrypt TInt (V TInt n) ct == Just bs
    it "roundtrips for String" $ property $ \(s :: String) bs ->
      let ct = encrypt TString bs
       in decrypt TString (V TString s) ct == Just bs
    it "roundtrips for (Int, String)" $ property $ \(a :: Int) (b :: String) bs ->
      let ty = TPair TInt TString
          val = V ty (a, b)
          ct = encrypt ty bs
       in decrypt ty val ct == Just bs
    it "roundtrips for [Int]" $ property $ \(xs :: [Int]) bs ->
      let ty = TList TInt
          val = V ty xs
          ct = encrypt ty bs
       in decrypt ty val ct == Just bs
    it "roundtrips for nested (Int, [String])" $
      property $ \(n :: Int) (xs :: [String]) bs ->
        let ty = TPair TInt (TList TString)
            val = V ty (n, xs)
            ct = encrypt ty bs
         in decrypt ty val ct == Just bs
    it "matches Bool" $ property $ \b ->
      matches (V TBool b) TBool
    it "matches Pair" $ property $ \a b ->
      matches (V (TPair TInt TString) (a, b)) (TPair TInt TString)
    it "matches List" $ property $ \xs ->
      matches (V (TList TInt) xs) (TList TInt)
    it "roundtrips for Bool" $ property $ \(b :: Bool) bs ->
      let ct = encrypt TBool bs
       in decrypt TBool (V TBool b) ct == Just bs
    it "roundtrips for Pair" $ property $ \(a :: Int) (b :: String) bs ->
      let t = TPair TInt TString
          ct = encrypt t bs
       in decrypt t (V t (a, b)) ct == Just bs
    it "roundtrips for List" $ property $ \(xs :: [Int]) bs ->
      let t = TList TInt
          ct = encrypt t bs
       in decrypt t (V t xs) ct == Just bs
