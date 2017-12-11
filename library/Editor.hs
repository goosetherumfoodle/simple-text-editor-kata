{-# LANGUAGE OverloadedStrings #-}

module Editor (main, perform, initialState, performAll) where

import Data.Text.Lazy (Text
                      , dropEnd
                      , append
                      , index)
import qualified Data.Text.Lazy as T
import Data.Int (Int64)
import Data.Dequeue (popRight, dropLeft, pushLeft, emptyDequeue)
import Types

main :: History -> Output
main = getOutput . (performAll initialState)

initialState :: State
initialState = State "" emptyDequeue emptyDequeue

-- loops `perform` on each command in history while threading state through
performAll :: State -> History -> State
performAll s hist | Just (cmds, cmd) <- popRight hist = performAll (perform s cmd) cmds
                  | otherwise = s

-- core editing logic.
perform :: State -> Command -> State
-- append to internal string, and save to history list
perform s cmd@(Append str) = State (getInternal s `append` str)
                                   (getOutput s)
                                   (cmd `pushLeft` (getHistory s))

-- delete from internal string, and save to history list
perform s cmd@(Delete i) = State (delete (fromIntegral i) $ getInternal s)
                                 (getOutput s)
                                 (cmd `pushLeft` (getHistory s))

-- push char to output string, but don't save to history
perform s (Print i) | Just char <- internalChar = State (getInternal s)
                                                        (char `pushLeft` (getOutput s))
                                                        (getHistory s) -- don't add Print to history!
                    | otherwise = s
  where
    internalChar = getInternalChar (fromIntegral i) $ getInternal s

-- co-recursive call to performAll will re-generate the internal string.
-- could infinitly loop if Undo were added to the history in the call to performAll
perform s Undo = State (newInternal s)
                       (getOutput s)
                       (dropLeft $ getHistory s)
  where
    newInternal :: State -> InternalString
    newInternal = getInternal . (performAll initialState) . dropLeft . getHistory

-- account for bad index values in Delete
delete :: Int -> Text -> Text
delete i str | i < 1 = str
             | i > (fromIntegral $ T.length str) = ""
             | otherwise = dropEnd (toIntegral i) str

-- account for bad index values in Print, and empty string
getInternalChar :: Int -> Text -> Maybe Char
getInternalChar _ "" = Nothing
getInternalChar i str | i < 1 = Just $ index str 0
                      | i >= (fromIntegral $ T.length str) = Just $ T.last str
                      | otherwise = Just $ index str $ toIntegral $ i - 1

toIntegral :: Int -> Int64
toIntegral = fromInteger . toInteger
