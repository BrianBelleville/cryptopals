module S15 (pkcs7PaddingValidation) where

import S10

import qualified Data.ByteString as B

pkcs7PaddingValidation :: B.ByteString -> Maybe B.ByteString
pkcs7PaddingValidation b = if validPadding then Just $ unpad b else Nothing
  where validPadding = let rb = B.reverse b
                           head = B.head rb
                           chunk = B.take (fromIntegral head) rb
                       in
                        B.foldr (\x a -> (x == head) && a) True chunk
