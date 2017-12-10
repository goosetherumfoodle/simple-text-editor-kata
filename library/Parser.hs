module Parser (main) where

import Data.Text.Lazy (pack)
import Text.Parser.Combinators (some)
import Text.Parser.Token (natural, whiteSpace)
import Data.ByteString.Char8 (ByteString)
import Control.Monad(replicateM)
import Control.Applicative ((<|>))
import Types
import Text.Trifecta (Parser
                     , Result
                     , parseByteString
                     , char
                     , space
                     , digit
                     , lower
                     , eof
                     , try
                     , manyTill
                     )

main :: ByteString -> Result [Command]
main input = parseByteString commandInputParser mempty input

-- loops parseCommand until end of file or bad input
commandInputParser :: Parser [Command]
commandInputParser = do
  _ <- parseSomeDigits 1 <* whiteSpace -- throw away first digit as we don't use it
  manyTill parseCommand $ try endOfLine

parseCommand :: Parser Command
parseCommand = do
  commandNumber <- digit
  -- we're assuming valid input commands of 1-4
  case commandNumber of
    '1' -> Append . pack <$> parseStringArg
    '2' -> Delete . fromInteger <$> parseNumberArg
    '3' -> Print . fromInteger <$> parseNumberArg
    _ -> whiteSpace >> return Undo

parseNumberArg :: Parser Integer
parseNumberArg = parseToken natural

parseStringArg :: Parser String
parseStringArg = parseToken $ some lower

parseToken :: Parser a  -> Parser a
parseToken token = some space >> token >>= \args -> return args <* whiteSpace

parseSomeDigits :: Int -> Parser Integer
parseSomeDigits num = read <$> replicateM num digit

-- detect the end of the line or the file
endOfLine :: Parser (Either () Char)
endOfLine = Right <$> (char '\n') <|> Left <$> eof
