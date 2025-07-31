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

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["encrypt"] -> do
      ct <- encrypt TInt plaintext
      putStrLn (hex ct)
    ["decrypt", hexCt] -> do
      let ct = unhex hexCt
      case decrypt TInt (V TInt (0 :: Int)) ct of
        Just pt -> C.putStrLn pt
        Nothing -> putStrLn "FAIL"
    _ -> putStrLn "usage: encrypt|decrypt [hex]"
