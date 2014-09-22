module S15 (pkcs7PaddingValidation) where

import S10

import qualified Data.ByteString as B

pkcs7PaddingValidation :: Integral i => i -> B.ByteString -> Maybe B.ByteString
pkcs7PaddingValidation block_size b = if validPadding then Just $ unpad b else Nothing
  where validPadding = let rb = B.reverse b
                           head = B.head rb
                           chunk = B.take (fromIntegral head) rb
                       in
                        if (fromIntegral head) > block_size
                        then False -- if the pad length is greater than a block size, automatically fail 
                        else B.foldr (\x a -> (x == head) && a) True chunk -- else check if all are same as head
