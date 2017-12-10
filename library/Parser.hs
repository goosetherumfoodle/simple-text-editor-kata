module Parser where

import Data.Text.Lazy (pack)
import Data.Text.Encoding (decodeUtf8)
import Text.Trifecta (Parser, Result, parseByteString, many, char, space, string, digit, lower, eof, try, manyTill)
import Text.Parser.Combinators (some)
import Text.Parser.Token (natural, whiteSpace)
import Prelude hiding (readFile)
import Data.ByteString.Char8 (ByteString , readFile)
import Editor (Command(..))
import Control.Monad(replicateM)
import Control.Applicative ((<|>))

-- TODO: delete example file
  -- extract types into seperate namespace

parseCommandInput :: IO (Result [Command])
parseCommandInput = parseByteString commandInputParser mempty <$> commandInput

commandInputParser :: Parser [Command]
commandInputParser = do
  _ <- parseSomeDigits 1 <* char '\n' -- throw away first digit as we don't need it
  manyTill parseCommand $ try endOfLine

parseCommand :: Parser Command
parseCommand = do
  commandNumber <- digit
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

commandInput :: IO ByteString
commandInput = readFile "data/input.txt"

parseSomeDigits :: Int -> Parser Integer
parseSomeDigits num = read <$> replicateM num digit

endOfLine :: Parser (Either () Char)
endOfLine = Right <$> (char '\n') <|> Left <$> eof
