{-# LANGUAGE OverloadedStrings #-}

module Editor (main, perform, initialState, performAll) where

import Data.Text.Lazy (Text
                      , dropEnd
                      , append
                      , index)
import qualified Data.Text.Lazy as T
import Data.Int (Int64)
import Data.Queue (emptyQueue, queuePush)
import Data.Dequeue (popRight, dropLeft, pushLeft, emptyDequeue)
import Types

main :: History -> Output
main = getOutput . (performAll initialState)

initialState :: State
initialState = State "" emptyQueue emptyDequeue

-- loops perform on each command in history while threading state through
performAll :: State -> History -> State
performAll s hist | Just (cmds, cmd) <- popRight hist = performAll (perform s cmd) cmds
                  | otherwise = s

-- core function for editing logic.
perform :: State -> Command -> State
-- append to internal string, and save to history list
perform s cmd@(Append str) = State (append (getInternal s) str)
                                   (getOutput s)
                                   (cmd `pushLeft` (getHistory s))

-- delete from internal string, and save to history list
perform s cmd@(Delete i) = State (delete (fromIntegral i) (getInternal s))
                                 (getOutput s)
                                 (cmd `pushLeft` (getHistory s))

-- push char to output string, but don't save to history
perform s (Print i) = State (getInternal s)
                            (getInternalChar (fromIntegral i) (getInternal s) `queuePush` (getOutput s))
                            (getHistory s) -- don't add Print to history!

-- co-recursive call to performAll will re-generate the internal string.
-- could infinitly loop if Undo were added to the history in the call to performAll
perform s Undo = State (newInternal s)
                       (getOutput s)
                       (dropLeft $ getHistory s)
  where
    newInternal :: State -> InternalString
    newInternal = getInternal . (performAll initialState) . dropLeft . getHistory -- reversed to play history in correct order

-- account for bad index values in Delete
delete :: Int -> Text -> Text
delete i str | i < 1 = str
             | i > (fromIntegral $ T.length str) = ""
             | otherwise = dropEnd (toIntegral i) str

-- account for bad index values in Print
getInternalChar :: Int -> Text -> Char
getInternalChar i str | i < 1 = index str 0
                      | i >= (fromIntegral $ T.length str) = T.last str
                      | otherwise = index str $ toIntegral $ i - 1

toIntegral :: Int -> Int64
toIntegral = fromInteger . toInteger
