import           S10
import           S11
import           S15

import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import           Data.Word
import           Debug.Trace
import           System.Random

bstrings = map (B64.decodeLenient . C8.pack)
           ["MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc=",
           "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic=",
           "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw==",
           "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg==",
           "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl",
           "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA==",
           "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw==",
           "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8=",
           "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g=",
           "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"]

key = randByteString (mkStdGen 7812768345) 16
iv = randByteString (mkStdGen 464378932) 16

paddingOracle iv c = result $ pkcs7PaddingValidation $ cbcDecryptRaw key iv c
  where result (Just _) = True
        result Nothing = False

target = cbcEncrypt key iv $ bstrings !! randIndex
  where (randIndex, _) = randomR (0, length bstrings) (mkStdGen 1235)

attack iv ctext = doAttack B.empty ctext
  where doAttack p ct 
          | B.null ct = p
          | otherwise = let (targetBlock, rest) = B.splitAt 16 ct
                        in
                         doAttack (B.append p
                                   (decryptBlock iv targetBlock)) rest

decryptBlock :: B.ByteString -> B.ByteString -> B.ByteString
decryptBlock iv c = doDecrypt B.empty
  where doDecrypt k
          | B.length k == B.length c = k
          | otherwise = doDecrypt (B.cons (decryptByte iv c k) k)

decryptByte :: B.ByteString -> B.ByteString -> B.ByteString -> Word8
decryptByte iv c k = doDecryptByte (maxBound :: Word8)
  where doDecryptByte 0 = findResult 0
        doDecryptByte g = if paddingOracle (tamperIv g) c
                          then  findResult g 
                          else doDecryptByte (g - 1)
        tamperIv g = let end = bxor (B.replicate l (fromIntegral pad)) k
                         tamper = B.append
                                  (B.snoc (B.replicate (16 - pad) 0) g)
                                  end
                     in bxor iv tamper
        findResult g = let index = 16 - pad
                           ivb = B.index iv index
                       in g `xor` ivb `xor` (fromIntegral pad)
        l = B.length k
        pad = l + 1

main = putStrLn $ show $ attack iv target
