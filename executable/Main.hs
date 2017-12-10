{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Trifecta.Result (Result(..))
import qualified Data.ByteString.Char8 as BS
import qualified Editor
import qualified Parser
import qualified Data.Queue as Q
import Types

main :: IO ()
main = do
  fileText <- (BS.readFile <$> getFilePath)
  parsedCommands <- handleResults <$> (Parser.main <$> fileText)
  parsedCommands >>= (Q.printQueue . Editor.main)

handleResults :: Result [Command] -> IO [Command]
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
