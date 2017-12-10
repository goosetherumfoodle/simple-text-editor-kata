{-# LANGUAGE OverloadedStrings #-}

module Editor (main, perform, initialState, performAll) where

import Data.Text.Lazy (Text
                      , dropEnd
                      , append
                      , index)
import qualified Data.Text.Lazy as T
import Safe (tailSafe)
import Data.Int (Int64)
import Data.Queue (emptyQueue, queuePush)
import Types

main :: [Command] -> Output
main = getOutput . (performAll initialState)

initialState :: State
initialState = State "" emptyQueue []

performAll :: State -> [Command] -> State
performAll s (cmd:cmds) = performAll (perform s cmd) cmds
performAll s [] = s

-- core function for editing logic.
perform :: State -> Command -> State
-- append to internal string, and save to history list
perform s cmd@(Append str) = State (append (getInternal s) str)
                                   (getOutput s)
                                   (cmd : (getHistory s))

-- delete from internal string, and save to history list
perform s cmd@(Delete i) = State (delete (fromIntegral i) (getInternal s))
                                 (getOutput s)
                                 (cmd : getHistory s)

-- push char to output string, but don't save to history
perform s (Print i) = State (getInternal s)
                            (getInternalChar (fromIntegral i) (getInternal s) `queuePush` (getOutput s))
                            (getHistory s) -- don't add Print to history!

-- co-recursive call to performAll will re-generate the internal string.
-- could infinitly loop if Undo were added to the history in the call to performAll
perform s Undo = State (getInternal $ performAll initialState $ reverse $ tailSafe $ getHistory s) -- don't add Undo to history!
                       (getOutput s)
                       (tailSafe $ getHistory s)

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
