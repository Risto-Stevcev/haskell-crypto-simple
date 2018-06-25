module Crypto.Simple.CTR where

import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Crypto.Cipher.Types (makeIV)
import Crypto.Random.Entropy (getEntropy)
import Crypto.Data.Padding (pad, Format(PKCS7))
import Crypto.Simple.Cipher (encrypt', decrypt', blockLength, CipherMode(..))
import qualified Data.ByteString as B


-- | The key must be less than 32 bytes in length
encrypt :: ByteString -> ByteString -> IO ByteString
encrypt key msg = do
    entropy <- getEntropy blockLength :: IO ByteString
    pure $ entropy <> (encrypt' CTR key' (makeIV entropy) msg)
  where
    key' = if B.length key == 16 || B.length key == 32 then key else pad (PKCS7 blockLength) key


-- | The key must be less than 32 bytes in length
decrypt :: ByteString -> ByteString -> IO ByteString
decrypt key msg = pure $ decrypt' CTR key' (makeIV iv) msg'
  where
    (iv, msg') = B.splitAt blockLength msg
    key' = if B.length key == 16 || B.length key == 32 then key else pad (PKCS7 blockLength) key
