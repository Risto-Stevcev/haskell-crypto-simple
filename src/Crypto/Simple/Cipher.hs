module Crypto.Simple.Cipher where

import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), IV)
import Crypto.Error (CryptoFailable(..))

newtype Key a = Key ByteString
    deriving (Show,Eq)

blockLength :: Int
blockLength = 16

maxKeyLength :: Int
maxKeyLength = 32

data CipherMode = CBCencrypt | CBCdecrypt | CTR


-- | key must be 32 bytes in length
encrypt' :: CipherMode -> ByteString -> Maybe (IV AES256) -> ByteString -> ByteString
encrypt' mode key iv = cipherEncrypt ctx iv'
  where
    cipherEncrypt = case mode of
      CBCencrypt -> cbcEncrypt
      CBCdecrypt -> cbcDecrypt
      CTR -> ctrCombine
    ctx = cipherInitNoErr blockCipher
    iv' = maybe (error $ "IV length must be " <> show blockLength <> " bytes") id iv
    blockCipher = cipherMakeKey (undefined :: AES256) key
    cipherInitNoErr :: BlockCipher c => Key c -> c
    cipherInitNoErr (Key k) = case cipherInit k of
      CryptoPassed a -> a
      CryptoFailed e -> error (show e)
    cipherMakeKey :: Cipher cipher => cipher -> ByteString -> Key cipher
    cipherMakeKey _ = Key


decrypt' :: CipherMode -> ByteString -> Maybe (IV AES256) -> ByteString -> ByteString
decrypt' = encrypt'
