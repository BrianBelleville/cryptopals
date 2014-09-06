module S10 (cbcEncrypt,
            cbcDecrypt,
            cbcDecryptRaw,
            ecbEncrypt,
            ecbDecrypt,
            unpad,
            bxor) where 

import           Crypto.Cipher.AES
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import           Data.Word
import           S9

pad :: B.ByteString -> B.ByteString
pad = flip pkcs7Padding $ 16

unpad :: B.ByteString -> B.ByteString
unpad b = let br = B.reverse b in
  B.reverse (B.drop (fromIntegral $ B.head br) br)

bsize = 16

bxor :: B.ByteString -> B.ByteString -> B.ByteString
bxor l r = B.pack $ map (\ (x,y) -> x `xor` y) $ B.zip l r
        
      
doCbc :: AES -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
doCbc cipher iv ctext ptext = if B.null ptext then ctext else
                                  let
                                    (p,r) = B.splitAt bsize ptext
                                    px = bxor p iv
                                    cn = encryptECB cipher px
                                  in doCbc cipher cn (B.append ctext cn) r


cbcEncrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
cbcEncrypt key iv pt = doCbc (initAES key) iv B.empty (pad pt)

-- assumes that the iv is the first block of the ctext, and drops that
-- block
doCbcDecrypt :: AES -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
doCbcDecrypt cipher iv ctext ptext = if B.null ctext then ptext else
                                       let
                                         (c,r) = B.splitAt bsize ctext
                                         pn = bxor (decryptECB cipher c) iv
                                       in doCbcDecrypt cipher c r $ B.append ptext pn

cbcDecrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
cbcDecrypt key iv ct = unpad $ cbcDecryptRaw  key iv ct

cbcDecryptRaw key iv ct = doCbcDecrypt (initAES key) iv ct B.empty

ecbEncrypt :: B.ByteString -> C8.ByteString -> C8.ByteString
ecbEncrypt key ptext = encryptECB (initAES key) (pad ptext)

ecbDecrypt :: B.ByteString -> C8.ByteString -> C8.ByteString
ecbDecrypt key ctext = unpad $ decryptECB (initAES key) ctext

key = C8.pack "YELLOW SUBMARINE"
iv = B.pack(take 16 $ (repeat 0 :: [Word8]))


doit = do
  f <- B.readFile "10.txt"
  let fb = B64.decodeLenient f
  return $ cbcDecrypt key iv fb
