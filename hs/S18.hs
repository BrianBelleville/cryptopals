{-# LANGUAGE FlexibleInstances #-}
-- flexible instances needed to implement type class instance for
-- String

import S10

import           Crypto.Cipher.AES
import           Data.Binary.Get
import           Data.Binary.Put
import Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Char8 as SC8
import           Data.LargeWord
import           Data.Word

class AsBstring a where
  asBstring :: a -> B.ByteString

instance AsBstring B.ByteString where
  asBstring a = a

instance AsBstring String where
  asBstring a = C8.pack a

instance AsBstring Word64 where
  asBstring a = runPut $ putWord64le a

instance AsBstring Word128 where
  asBstring (LargeKey a b) = runPut putThem
    where putThem = do
            putWord64le b
            putWord64le a

class AsW64 a where
  asW64 :: a -> Word64

instance AsW64 B.ByteString where
  asW64 a = runGet getWord64le a

newtype MyIntegral a = MyIntegral a

instance Integral a => AsW64 (MyIntegral a) where
  asW64 (MyIntegral a) = fromIntegral a :: Word64

instance AsW64 Word64 where
  asW64 a = a

instance AsW64 String where
  asW64 a = asW64 $ asBstring a

makeKeyStream :: (AsBstring a, AsW64 b0, AsW64 b1) => a -> b0 -> b1 -> B.ByteString
makeKeyStream key nonce ctr = stream bigCtr
  where cipher = initAES $ B.toStrict $ asBstring key
        bigCtr = LargeKey (asW64 nonce) (asW64 ctr)
        stream bc = B.append
                    (C8.fromStrict $ encryptECB cipher (B.toStrict $ asBstring bc))
                    (stream (bc + 1))

startKeyStream key nonce = makeKeyStream key nonce (0 :: Word64)

doCTR key nonce cipherText = (B.toStrict cipherText) `bxor` (B.toStrict keystream)
  where keystream = B.take (B.length cipherText) $ (startKeyStream key nonce)

ctrEncrypt a b c = doCTR a b c

ctrDecrypt a b c = doCTR a b c

target = C8.fromStrict $ B64.decodeLenient $ SC8.pack "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="

plaintext = ctrDecrypt "YELLOW SUBMARINE" (0 :: Word64) target
