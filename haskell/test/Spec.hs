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
    it "matches Bool" $ property $ \b ->
      matches (V TBool b) TBool
    it "matches Pair" $ property $ \a b ->
      matches (V (TPair TInt TString) (a,b)) (TPair TInt TString)
    it "matches List" $ property $ \xs ->
      matches (V (TList TInt) xs) (TList TInt)
