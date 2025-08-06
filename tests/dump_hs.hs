import qualified Data.ByteString as B
import Numeric (showHex)
import Types

hex :: B.ByteString -> String
hex bs = concatMap twoHex (B.unpack bs)
  where
    twoHex n = let h = showHex n "" in if length h == 1 then '0' : h else h

printInfo :: (String, Type a) -> IO ()
printInfo (name, ty) = do
  let bytes = canonicalBytes ty
      -- keyFromType now derives keys via HKDF-SHA256 with fixed salt/info
      key = keyFromType ty
  putStrLn $ "{\"type\":\"" ++ name ++ "\",\"bytes\":" ++ show (B.unpack bytes) ++ ",\"key\":\"" ++ hex key ++ "\"}"

main :: IO ()
main =
  mapM_
    printInfo
    [ ("int", TInt),
      ("str", TString),
      ("pair", TPair TInt TBool),
      ("list", TList TInt)
    ]
