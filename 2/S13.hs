import Text.ParserCombinators.Parsec
import qualified Data.Map as M

record :: GenParser Char st (M.Map String String)
record = empty <|> pairs
          
empty = do eof
           return M.empty

pairs =
  do (k,v) <- pair
     rest <- remainingPairs
     return $ M.insert k v rest

pair =  do
  k <- str
  char '='
  v <- str
  return (k,v)

str = many (noneOf "&=")

remainingPairs =
  (char '&' >> record)
  <|> (return M.empty)
  
parseRecord input = parse record "user" input

