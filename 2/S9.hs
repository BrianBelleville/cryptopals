module S9 ( pkcs7Padding ) where

import qualified Data.ByteString as B
import           Data.Word

pkcs7Padding :: B.ByteString -> Int -> B.ByteString
pkcs7Padding text blocksize = let l = B.length text
                                  rem = if l < blocksize then
                                          blocksize - l
                                        else
                                          blocksize - (l `mod` blocksize)
                                  padlen = if rem == 0 then
                                             blocksize
                                           else
                                             rem
                                  w8padlen = fromIntegral padlen :: Word8
                                  pad = B.pack (take padlen $ repeat w8padlen)
                            in
                             B.append text pad
                            
