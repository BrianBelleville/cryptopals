import           S10
import           S11

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Data.Hash
import qualified Data.Map as M
import           System.Random
import           Text.ParserCombinators.Parsec

  -- Parser
record :: GenParser Char st (M.Map String String)
record = do t <- pairs
            eof
            return $ M.fromList t

pairs = sepBy pair (char '&')

pair =  do  k <- str
            char '='
            v <- str
            return (k,v)

str = many (noneOf "&=")

parseRecord input = parse record "user" input


key = randByteString (mkStdGen 847391275) 16

profileFor address = "email=" ++ cleanAddress ++
                     "&uid=" ++ (show uid) ++
                     "&role=user"
  where cleanAddress = filter (\x -> and [(x /= '=') , (x /= '&')]) address
        uid = asWord64 (hash cleanAddress) `mod` 100

processToken token = case parseRecord plainText of
  Right r -> Just r
  Left _ -> Nothing
  where plainText = C8.unpack $ ecbDecrypt key token


data Group = User | Admin deriving (Show)

getGroup :: M.Map [Char] [Char] -> Maybe Group
getGroup r = do test <- M.lookup "role" r
                case test of "admin" -> return Admin
                             "user" -> return User
                             _ -> Nothing


-- The user interface functions, atacker can only call these 2 functions
                             
createToken address = ecbEncrypt key cookie
  where cookie = C8.pack $ profileFor address

-- Only give attacker success or fail
captureTheFlag ctext = messageFor (processToken ctext >>= getGroup)
  where messageFor (Just Admin) = "You Won!"
        messageFor _ = "Sorry Sucker"


-- And this kills it
attack = let
  bomb = B.append (C8.pack "admin") (B.pack $ take 11 $ repeat 11)
  test = B.append (B.pack $ take 10 $ repeat 11) bomb
  scary = B.take 16 $ B.drop 16 $ createToken $ C8.unpack test
  sheep = createToken "brianb@me.com"
  waitForIt = B.reverse $ B.drop 16 $ B.reverse sheep
  wolf = B.append waitForIt scary
  in
   captureTheFlag wolf
