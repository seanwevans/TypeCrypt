{-# LANGUAGE GADTs #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Numeric (readHex, showHex)
import System.Environment (getArgs)
import Types

hex :: B.ByteString -> String
hex bs = concatMap twoHex (B.unpack bs)
  where
    twoHex n = let h = showHex n "" in if length h == 1 then '0' : h else h

unhex :: String -> B.ByteString
unhex [] = B.empty
unhex (a : b : rest) =
  let [(n, _)] = readHex [a, b]
   in B.cons n (unhex rest)
unhex _ = error "invalid hex"

plaintext :: B.ByteString
plaintext = C.pack "cross-test"

data TV where
  TV :: Type a -> Value -> TV

parseType :: String -> Maybe TV
parseType "int" = Just (TV TInt (V TInt (0 :: Int)))
parseType "str" = Just (TV TString (V TString ""))
parseType "pair" = Just (TV (TPair TInt TBool) (V (TPair TInt TBool) ((0 :: Int), False)))
parseType _ = Nothing

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["encrypt", tyName] -> case parseType tyName of
      Just (TV ty _) -> do
        result <- encrypt ty plaintext
        either (putStrLn . ("FAIL: " ++) . show) (putStrLn . hex) result
      Nothing -> putStrLn "unknown type"
    ["decrypt", tyName, hexCt] -> case parseType tyName of
      Just (TV ty val) -> do
        let ct = unhex hexCt
        case decrypt ty val ct of
          Right pt -> C.putStrLn pt
          Left _ -> putStrLn "FAIL"
      Nothing -> putStrLn "unknown type"
    _ -> putStrLn "usage: encrypt|decrypt <type> [hex]"
