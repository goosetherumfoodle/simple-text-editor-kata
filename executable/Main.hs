{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Trifecta.Result (Result(..))
import qualified Data.ByteString.Char8 as BS
import qualified Editor
import qualified Parser
import qualified Data.Dequeue as D
import Types

-- will read file from commandline if provided,
-- otherwise uses a default sample file
main :: IO ()
main = do
  fileText <- (BS.readFile <$> getFilePath)
  parsedCommands <- handleResults <$> (Parser.main <$> fileText)
  parsedCommands >>= (printDequeue . Editor.main)

printDequeue :: Show a => D.Dequeue a -> IO ()
printDequeue deq | Just (left, x) <- D.popRight deq = print x >> printDequeue left
                 | otherwise = return ()

-- Throws error if parser didn't like the file
handleResults :: Result History -> IO History
handleResults (Success cmds) = return cmds
handleResults (Failure err) =  do BS.hPutStrLn stderr $ BS.pack ("Parsing error: " ++ (show err))
                                  exitFailure

getFilePath :: IO String
getFilePath = getArgs >>= firstArg where
  firstArg :: [String] -> IO String
  firstArg (path:_) = return path
  firstArg _        = return defaultFilePath

defaultFilePath :: String
defaultFilePath = "data/input-sample.txt"
