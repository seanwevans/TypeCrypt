import Test.Hspec
import Test.QuickCheck
import Types

main :: IO ()
main = hspec $ do
  describe "matches" $ do
    it "returns True for matching Type and Value" $ property $ \n ->
      matches (V TInt n) TInt
    it "returns False for mismatched Type" $ property $ \n ->
      not (matches (V TInt n) TString)

  describe "encrypt/decrypt" $ do
    it "roundtrips for Int" $ property $ \(n :: Int) bs ->
      let ct = encrypt TInt bs
       in decrypt TInt (V TInt n) ct == Just bs
    it "roundtrips for String" $ property $ \(s :: String) bs ->
      let ct = encrypt TString bs
       in decrypt TString (V TString s) ct == Just bs
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
