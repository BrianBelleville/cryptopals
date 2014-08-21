import           S10
import           S11

import Debug.Trace
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import           Data.Word
import           System.Random
import qualified Data.Map as M

secret :: B.ByteString
secret = case B64.decode $ C8.pack "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK" of
           Right s -> s
           Left s -> B.empty

key :: B.ByteString
key = randByteString (mkStdGen 12390875) 16

oracle :: B.ByteString -> B.ByteString
oracle b = ecbEncrypt key (B.append b secret)

detectBsize :: (B.ByteString -> B.ByteString) -> Int
detectBsize o = doDetect 1 (B.length $ o B.empty)
  where doDetect c i = let l = B.length $ o (getText c)
                       in if l /= i
                             then gcd l i
                             else doDetect (c + 1) i

getText n = C8.pack (take n (repeat 'a'))

crack :: (B.ByteString -> B.ByteString) -> Maybe B.ByteString
crack o = let crack = crackECB $ detectBsize o
          in case algoDetector o of
               ECB -> Just $ crack o
               CBC -> Nothing

crackECB :: Int -> (B.ByteString -> B.ByteString) -> B.ByteString
crackECB b o = C8.pack "I can crack it"

breakBlock :: Int
              -> (B.ByteString -> B.ByteString)
              -> Maybe [Word8]
breakBlock b o = loop 16 $ Just []
  where loop _ Nothing  = Nothing
        loop 0 r = r
        loop c (Just r) = case breakByte b o r M.empty 255 of 
          Just w -> loop (c - 1) $ Just $ r ++ [w]
          Nothing -> Nothing

breakByte :: Int 
          -> (B.ByteString -> B.ByteString) 
          -> [Word8]
          -> M.Map  B.ByteString Word8
          -> Word8 
          -> Maybe Word8
breakByte b o k m c 
  | c == 0 = let real = B.take b $ o blob
             in M.lookup real m
  | otherwise = let map = M.insert block c m
                in  breakByte b o k map (c - 1)
  where blob = getText (b - (1 + length k))
        text = B.append blob $ B.pack $ k ++ [c]
        block = B.take b $ o text

