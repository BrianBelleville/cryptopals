import           S10
import           S11

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           System.Random

prefix = C8.pack "comment1=cooking%20MCs;userdata="
suffix = C8.pack ";comment2=%20like%20a%20pound%20of%20bacon"

key = randByteString (mkStdGen 87123) 16
iv = randByteString (mkStdGen 788234) 16

encryptThing b = cbcEncrypt key iv (B.append prefix (B.append cleanString suffix))
  where cleanString = B.foldl' quote B.empty b
        quote a 0x3b = B.append a $ C8.pack "%3B"
        quote a 0x3d = B.append a $ C8.pack "%3D"
        quote a w = B.snoc a w

decryptThing = cbcDecrypt key iv

testThing c = let ptext = decryptThing c in
               (C8.pack "admin=true") `B.isInfixOf` ptext 

evilPayload = let target = C8.pack ";admin=true;bb=l"
                  zeroBlock = B.replicate 16 0
                  benign = encryptThing zeroBlock
                  mask = B.append zeroBlock (B.append
                                             target
                                             (B.replicate ((B.length benign) - 32) 0))
              in
               bxor benign mask

didIWin = if testThing evilPayload then "yes" else "nope"
                  
