import           S12
import           S11

import qualified Data.ByteString as B
import qualified Data.Set as S
import           System.Random

prefix = randByteString gen len
  where (len, gen) = randomR (0,48) $ mkStdGen 203988934

oracle14 b = oracle (B.append prefix b)

hasBlock block bstring  = doLoop bstring 0
  where doLoop bstring num =
          let (firstB, rest) = B.splitAt (B.length block) bstring in
             if B.null bstring
             then Nothing
             else if firstB == block
                  then Just num
                  else doLoop rest (num + 1)

getFirstDupBlock b = doFind b S.empty
  where doFind b s = if B.null b then Nothing
                     else
                       let (h,t) = B.splitAt 16 b
                       in
                        if S.member h s then Just h
                        else
                          doFind t $ S.insert h s

zeroBlock o = let Just b = getFirstDupBlock $ o $ B.replicate 64 0
              in b

cleanUpOracle o = let (padLen, blocks) = findIt 0
                      randChunkLen = blocks * 16 in
                    \b -> B.drop randChunkLen $ o $ B.append (B.replicate padLen 0) b
  where findIt num = case hasBlock (zeroBlock o) (o $ B.replicate (16 + num) 0)
                     of 
                      Just n -> (num,n)
                      Nothing -> findIt (num + 1)
        
plainText = crack $ cleanUpOracle oracle14
