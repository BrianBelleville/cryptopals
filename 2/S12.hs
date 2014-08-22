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
crackECB b o = target o b []

target :: (B.ByteString -> B.ByteString) -> Int -> [Word8] -> B.ByteString
target o b k
  | (B.length $ o B.empty) == length k = B.pack k
  | otherwise = target o b $ k ++ [next]
  where next = case M.lookup (getB o b k) (getGuesses o b k) of
          Just x -> x
          Nothing -> 0

getGuesses :: (B.ByteString -> B.ByteString)
           -> Int
           -> [Word8]
           -> M.Map B.ByteString Word8
getGuesses o b k = guess 255 M.empty
  where nn = b - 1
        p = reverse $ take nn $ reverse k
        addon = B.append (getText (nn - (length p))) $ B.pack p
        guess 0 m = m
        guess c m = guess (c - 1) $ M.insert (B.take b (o (B.snoc addon c))) c m


getB :: (B.ByteString -> B.ByteString)
     -> Int
     -> [Word8]
     -> B.ByteString
getB o b k
  | l >= b = B.take b $ B.drop ig $ o pad
  | otherwise = B.take b $ o (getText (b - (1 + l)))
  where l = length k
        n = l `div` b
        ig = n * b
        pad = getText (b - (l - (n*b) + 1))
        
