module Crypto.SimpleSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Crypto.Simple.Cipher (blockLength, maxKeyLength)
import Crypto.Random.Entropy (getEntropy)
import qualified Data.ByteString as B
import qualified Crypto.Simple.CTR as CTR
import qualified Crypto.Simple.CBC as CBC


propNotSameMsg :: (ByteString -> ByteString -> IO ByteString) -> Property
propNotSameMsg encrypt =
    forAll (arbitrary :: Gen String) $ \strMsg ->
    forAll (suchThat (arbitrary :: Gen String) (\s -> length s <= maxKeyLength)) $ \strKey -> ioProperty $ do

    let msg = pack strMsg
        key = pack strKey
    secretMsg <- encrypt key msg
    pure $ secretMsg /= msg


propIsomorphic :: (ByteString -> ByteString -> IO ByteString)
               -> (ByteString -> ByteString -> IO ByteString) -> Property
propIsomorphic encrypt decrypt = 
    forAll (arbitrary :: Gen String) $ \strMsg ->
    forAll (suchThat (arbitrary :: Gen String) (\s -> length s <= maxKeyLength)) $ \strKey -> ioProperty $ do

    let msg = pack strMsg
        key = pack strKey
    secretMsg <- encrypt key msg
    msg' <- decrypt key secretMsg
    pure $ msg' == msg


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "encrypt" $ do
        it "should not be the same as the original message (CBC)" $
            propNotSameMsg CBC.encrypt
        it "should not be the same as the original message (CTR)" $
            propNotSameMsg CTR.encrypt

    describe "encrypt/decrypt" $ do 
        it "should have an isomorphic relationship (CBC)" $
            propIsomorphic CBC.encrypt CBC.decrypt
        it "should have an isomorphic relationship (CTR)" $
            propIsomorphic CTR.encrypt CTR.decrypt
