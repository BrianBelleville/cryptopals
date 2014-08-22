import qualified Data.Map as M
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
