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

paddingOracle iv c = result $ pkcs7PaddingValidation 16 $ cbcDecryptRaw key iv c
  where result (Just _) = True
        result Nothing = False

target = cbcEncrypt key iv $ bstrings !! randIndex
  where (randIndex, _) = randomR (0, (length bstrings) - 1) (mkStdGen 1235)

attack iv ctext = doAttack B.empty ctext iv
  where doAttack p ct prev
          | B.null ct = p
          | otherwise = let (targetBlock, rest) = B.splitAt 16 ct
                        in
                         doAttack (B.append p
                                   (decryptBlock prev targetBlock)) rest targetBlock

decryptBlock :: B.ByteString -> B.ByteString -> B.ByteString
decryptBlock iv c = iv `bxor` (attackIntermediate c B.empty)

attackIntermediate :: B.ByteString -> B.ByteString -> B.ByteString
attackIntermediate c k
  | B.length c == B.length k = k
  | otherwise = attackIntermediate c (B.cons (search 255) k)
  where search 0 = figureOut k 0
        search g = if paddingOracle (makeIV k g) c
                     then figureOut k g
                     else search (g - 1) 

makeIV k g = B.append zeros (B.cons g knownEnd)
  where padding = (B.length k) + 1
        zeros = B.replicate (16 - padding) 0
        knownEnd = let want = B.replicate (padding - 1) (fromIntegral padding) in
          want `bxor` k

figureOut :: B.ByteString -> Word8 -> Word8
figureOut k g = want `xor` g
  where want = fromIntegral $ (B.length k) + 1

main = putStrLn $ show $ attack iv target
