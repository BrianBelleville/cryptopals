import           Control.Monad.ST
import           Data.Bits
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.Word
import           System.Random

newtype MTGen = MTGen (V.Vector Word32, Int)

makeMTVec :: Int -> V.Vector Word32
makeMTVec s = runST $ do
  mt <- MV.new 624
  loop 0 mt
  V.unsafeFreeze mt
  where
    loop 0 v = do
      MV.unsafeWrite v 0 (fromIntegral s)
      loop 1 v
    loop 624 v = return ()
    loop x v  = do
      p <- MV.unsafeRead v (x - 1)
      -- MT[i] := lowest 32 bits of(1812433253 * (MT[i-1] xor (right shift by 30 bits(MT[i-1]))) + i)
      MV.unsafeWrite v x (1812433253 * (p `xor` (shift p (-30))) + (fromIntegral x))
      loop (x + 1) v

mkMTGen :: Int -> MTGen
mkMTGen s = MTGen (makeMTVec s, 0)

instance RandomGen MTGen where
  split g = let (s1, g') = next g
                (s2, _) = next g' in
            (mkMTGen s1, mkMTGen s2)
  genRange _ = (0, 2^32 - 1)
  next (MTGen (v, x)) = let v' = if x == 0 then genNums v else v in
                         (getNext v' x, MTGen (v', (x + 1) `mod` 624))

getNext :: V.Vector Word32 -> Int -> Int
getNext v x = let y = v V.! x
                  y' = y `xor` (shift y (-11))
                  y'' = y' `xor` ((shift y' 7) .&. 2636928640)
                  y''' = y'' `xor` ((shift y'' 15) .&. 4022730752)
              in fromIntegral (y''' `xor` (shift y''' (-18))) :: Int

genNums :: V.Vector Word32 -> V.Vector Word32
genNums v = runST $ do
  vm <- V.thaw v
  loop vm 0
  V.unsafeFreeze vm
  where loop vm 624 = return ()
        loop vm i = do
          mti <- MV.unsafeRead vm i
          mtip1 <- MV.unsafeRead vm ((i + 1) `mod` 624)
          mtip397 <- MV.unsafeRead vm ((i + 1) `mod` 624)
          let y = (mti .&. 0x80000000) + (mtip1 .&. 0x7fffffff)
              nvi = (mtip397 `xor` (shift y (-1)))
              nvf = if odd y then nvi `xor` 2567483615 else nvi
          MV.unsafeWrite vm i nvf
          loop vm (i + 1)


