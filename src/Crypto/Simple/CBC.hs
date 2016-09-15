module Crypto.Simple.CBC where

import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (makeIV)
import Crypto.Random.Entropy (getEntropy)
import Crypto.Data.Padding (pad, unpad, Format(PKCS7))
import Crypto.Simple.Cipher (encrypt', decrypt', blockLength, CipherMode(..))
import qualified Data.ByteString as B


-- | The key must be less than 32 bytes in length
encrypt :: ByteString -> ByteString -> IO ByteString
encrypt key msg = do
    entropy <- getEntropy blockLength :: IO ByteString
    pure $ entropy <> (encrypt' CBCencrypt key' (makeIV entropy) msg')
  where
    msg' = pad (PKCS7 blockLength) msg 
    key' = if B.length key == 16 || B.length key == 32 then key else pad (PKCS7 blockLength) key


-- | The key must be less than 32 bytes in length
decrypt :: ByteString -> ByteString -> IO ByteString
decrypt key msg = do
    entropy <- getEntropy blockLength :: IO ByteString
    let msg'' = decrypt' CBCdecrypt key' (makeIV iv) msg'
    pure $ maybe msg'' id (unpad (PKCS7 blockLength) msg'')
  where
    (iv, msg') = B.splitAt blockLength msg
    key' = if B.length key == 16 || B.length key == 32 then key else pad (PKCS7 blockLength) key
