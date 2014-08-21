import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Set as S
import           Data.Word
import           S10
import           System.Random

randByteString :: RandomGen g => g -> Int -> B.ByteString
randByteString g l = B.pack $ take l $ randoms g

randBlock :: RandomGen g => g -> B.ByteString
randBlock = flip randByteString $ 16

cbcEncryptRand ::
  RandomGen g => g -> B.ByteString -> B.ByteString
cbcEncryptRand g = let (g1, g2) = split g in
  cbcEncrypt (randByteString g1 16) (randByteString g2 16)

randPadding :: RandomGen g => g -> B.ByteString -> B.ByteString
randPadding g b = let (g0, g1) = split g
                      (l0, g0') = randomR (5,10) g0
                      (l1, g0'') = randomR (5,10) g0' in
                   B.append (randByteString g0'' l0)
                   $ B.append b (randByteString g1 l1)

ecbEncryptRand :: RandomGen g => g -> B.ByteString -> B.ByteString
ecbEncryptRand g = ecbEncrypt (randBlock g) 

data Algo = ECB | CBC deriving (Show, Eq, Ord)

instance Random Algo where
  randomR (lo,hi) g = if lo < hi then random g else
                        -- else lo == hi, or unspecified behavior
                        (lo, g)
  random g = let (b, g') = random g in
    if b then (ECB, g') else (CBC, g')

encryptionOracle :: RandomGen g => g -> B.ByteString -> B.ByteString
encryptionOracle g b = let (choice, g') = random g
                           (g1, g2) = split g'
                           string = randPadding g1 b
                       in
                        case choice of ECB -> ecbEncryptRand g2 string
                                       CBC -> cbcEncryptRand g2 string

findDupBlock :: B.ByteString -> Bool
findDupBlock b = doFind b S.empty
  where doFind b s = if B.null b then False
                     else
                       let (h,t) = B.splitAt 16 b
                       in
                        if S.member h s then True
                        else
                          doFind t $ S.insert h s
  
algoDetector :: RandomGen g => g -> (g -> B.ByteString -> B.ByteString) -> Algo
algoDetector g bb = if findDupBlock $ bb g ptext then ECB else CBC
  where ptext = B.pack $ take (16 * 3) $ repeat 0
