module Parser (main) where

import           Control.Applicative     ((<|>))
import           Data.ByteString.Char8   (ByteString)
import           Data.Dequeue            (endequeue)
import           Data.Text.Lazy          (pack)
import           Text.Parser.Combinators (some)
import           Text.Parser.Token       (natural, whiteSpace)
import           Text.Trifecta           (Parser, Result, char, digit, eof,
                                          lower, manyTill, parseByteString,
                                          space, try)
import           Types

main :: ByteString -> Result History
main input = parseByteString commandInputParser mempty input

-- loops parseCommand until end of file or bad input
commandInputParser :: Parser History
commandInputParser = do
  _ <- digit <* whiteSpace -- throw away first digit as we don't use it
  endequeue . reverse <$> (manyTill parseCommand $ try endOfLine)

parseCommand :: Parser Command
parseCommand = do
  commandNumber <- digit
  -- we're assuming valid input commands of 1-4, and commands 1-3 have args
  case commandNumber of
    '1' -> Append . pack <$> parseStringArg
    '2' -> Delete . fromInteger <$> parseNumberArg
    '3' -> Print . fromInteger <$> parseNumberArg
    _   -> whiteSpace >> return Undo

parseNumberArg :: Parser Integer
parseNumberArg = parseToken natural

parseStringArg :: Parser String
parseStringArg = parseToken $ some lower

parseToken :: Parser a -> Parser a
parseToken token = some space >> token >>= \args -> return args <* whiteSpace

-- detect the end of the line or the file
endOfLine :: Parser (Either () Char)
endOfLine = Right <$> (char '\n') <|> Left <$> eof
