{-# LANGUAGE OverloadedStrings #-}

module Editor (main
              , perform
               , Command(..)
               , State(..)
               , initialState
               , performAll
               ) where

import Data.Text.Lazy (Text
                      , dropEnd
                      , append
                      , index)
import qualified Data.Text.Lazy as T
import Data.Int (Int64)
import Safe (tailSafe)
import Data.Queue (emptyQueue, queuePush, Queue)

type Output = Queue Char
type InternalString = Text

data Command = Delete Int64 | Append Text | Print Int64 | Undo deriving (Show, Eq)
data State = State {
    getInternal :: InternalString
  , getOutput :: Output
  , getHistory :: [Command]
  } deriving (Show, Eq)

main :: [Command] -> Output
main = getOutput . (performAll initialState)

initialState :: State
initialState = State "" emptyQueue []

performAll :: State -> [Command] -> State
performAll s (cmd:cmds) = performAll (perform s cmd) cmds
performAll s [] = s

perform :: State -> Command -> State
perform s cmd@(Append str) = State (append (getInternal s) str)
                                   (getOutput s)
                                   (cmd : (getHistory s))
perform s cmd@(Delete i) = State (delete (fromIntegral i) (getInternal s))
                                 (getOutput s)
                                 (cmd : getHistory s)
perform s (Print i) = State (getInternal s)
                            (getInternalChar (fromIntegral i) (getInternal s) `queuePush` (getOutput s))
                            (getHistory s) -- don't add Print to history!
perform s Undo = State (getInternal $ performAll initialState (tailSafe $ getHistory s))
                       (getOutput s)
                       (tailSafe $ getHistory s)

delete :: Int -> Text -> Text
delete i str | i < 1 = str
             | i > (fromIntegral $ T.length str) = ""
             | otherwise = dropEnd (toIntegral i) str

getInternalChar :: Int -> Text -> Char
getInternalChar i str | i < 1 = index str 0
                      | i >= (fromIntegral $ T.length str) = T.last str
                      | otherwise = index str $ toIntegral $ i - 1

toIntegral :: Int -> Int64
toIntegral = fromInteger . toInteger
