import Test.QuickCheck
import Types

prop_matchesInt :: Int -> Bool
prop_matchesInt n = matches (VInt n) TInt

prop_matchesString :: String -> Bool
prop_matchesString s = matches (VString s) TString

main :: IO ()
main = do
  quickCheck prop_matchesInt
  quickCheck prop_matchesString
