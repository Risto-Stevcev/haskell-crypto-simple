# haskell-crypto-simple

A simple high level encryption interface based on cryptonite


## Usage

```haskell
> import Crypto.Simple.CBC (encrypt, decrypt) 
> import Data.ByteString.Char8 (pack)
> let key = pack "my secret key"
> let msg = pack "this is a message"
> encrypt key msg >>= \secretMsg -> decrypt key secretMsg
"this is a message"
```


## Implementation

The implementation is based on some good defaults

- The cipher that's used is AES 256
- You can choose between CBC or CTR cipher modes (modules Crypto.Simple.CBC and Crypto.Simple.CTR) 
- If you use CBC, it will pad/unpad the message using the PKCS7 padding scheme 
- The entropy method used for the initialization vector is the default system-level CSPRNG (ie. /dev/random). It is based on Crypto.Random.Entropy.getEntropy
- The maximum size for the encryption key is 32 bytes
