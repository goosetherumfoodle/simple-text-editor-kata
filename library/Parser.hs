module Parser (main) where

import Data.Text.Lazy (pack)
import Text.Parser.Combinators (some)
import Text.Parser.Token (natural, whiteSpace)
import Data.ByteString.Char8 (ByteString)
import Editor (Command(..))
import Control.Monad(replicateM)
import Control.Applicative ((<|>))
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

-- TODO: delete example file
  -- extract types into seperate namespace

main :: ByteString -> Result [Command]
main input = parseByteString commandInputParser mempty input

commandInputParser :: Parser [Command]
commandInputParser = do
  _ <- parseSomeDigits 1 <* char '\n' -- throw away first digit as we don't use it
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

endOfLine :: Parser (Either () Char)
endOfLine = Right <$> (char '\n') <|> Left <$> eof