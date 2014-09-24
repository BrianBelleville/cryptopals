import           S10
import           S11
import           S18

import qualified Data.ByteString as B
import           Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import           Data.Hex
import           Data.List
import qualified Data.Map as M
import           Data.Word
import           System.Random

defaultMap = M.fromList $ map (\x -> (x, 0)) [97..122]
  
idealFreqMap = M.fromList [
  (97,0.08167),                 -- a
  (98,0.01492),
  (99,0.02782),
  (100,0.04253),
  (101,0.12702),
  (102,0.02228),
  (103,0.02015),
  (104,0.06094),
  (105,0.06966),
  (106,0.00153),
  (107,0.00772),
  (108,0.04025),
  (109,0.02406),
  (110,0.06749),
  (111,0.07507),
  (112,0.01929),
  (113,0.00095),
  (114,0.05987),
  (115,0.06327),
  (116,0.09056),
  (117,0.02758),
  (118,0.00978),
  (119,0.02360),
  (120,0.00150),
  (121,0.01974),
  (122,0.00074) ]               -- z

-- this doesn't effect letters outside of the range [A-Z]
bsToLower = B.map (\x -> if x >= 0x41 && x <= 0x5a then x + 0x20 else x)

toCountMap = B.foldr (M.adjust (+1.0)) defaultMap

toFreqMap c = M.map (/(fromIntegral c))

diff l r = M.foldr (+) (0 :: Double)  $ M.intersectionWith (\x y -> abs (x - y)) l r
  
letterError :: B.ByteString -> Double
letterError b = diff idealFreqMap $ toFreqMap (B.length prepare) $ toCountMap prepare
  where prepare = B.filter (\x -> x >= 0x61 && x <= 0x7a) $ bsToLower b 

spaceError b = abs (ideal - spaceFraction)
  where l = B.length b
        ns = B.length $ B.filter (\x -> x /= 0x20) b
        spaceFraction = (fromIntegral ns) / (fromIntegral l)
        ideal = 1/6 :: Double

punctuationError b = if fraction > maxPunction then fraction - maxPunction else 0
  where l = B.length b
        a = B.length $ B.filter (\x -> (x >= 0x41 && x <= 0x7a) || x == 0x20) b
        fraction = (fromIntegral a) / (fromIntegral l)
        maxPunction = 1/10 :: Double

nonPrintableError b = B.foldr (\x b -> if (x < 0x20 || x > 0x7e) && x /= 0x0a then 1000 else b) 0 b

score b = letterError b + spaceError b + punctuationError b + nonPrintableError b
