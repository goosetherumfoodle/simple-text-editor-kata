{-# LANGUAGE OverloadedStrings #-}

module Editor (perform
               , Command(..)
               , State(..)
               , initialState
               , performAll
               , queueEmpty
               , enqueue) where

import Data.Text.Lazy (Text
                      , dropEnd
                      , append
                      , index)
import qualified Data.Text.Lazy as T
import Data.Int (Int64)
import Data.Sequence (Seq
                     , empty
                     , (<|)
                     , fromList)

-- TODO: 1: protect undoable command list (in state) from getting a non-undoable command
-- 4: move queue logic to own namespace

newtype Queue a = Queue (Seq a) deriving (Show, Eq)

type Output = Queue Char
type InternalString = Text

data Command = Delete Int64 | Append Text | Print Int64 | Undo deriving (Show, Eq)
data State = State {
    getInternal :: InternalString
  , getOutput :: Output
  , getHistory :: [Command]
  } deriving (Show, Eq)

enqueue :: [a] -> Queue a
enqueue = Queue . fromList

queuePush :: a -> Queue a -> Queue a
queuePush a (Queue sqnce) = Queue $ a <| sqnce

queueEmpty :: Queue a
queueEmpty = Queue empty

initialState :: State
initialState = State "" queueEmpty []

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
perform s Undo = State (getInternal $ performAll initialState (tail $ getHistory s))
                       (getOutput s)
                       (tail $ getHistory s)

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
