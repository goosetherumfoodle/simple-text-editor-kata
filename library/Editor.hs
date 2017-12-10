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
import Data.Int (Int64)
import Data.Sequence (Seq
                     , empty
                     , (<|)
                     , fromList)

-- TODO: 1: protect undoable command list (in state) from getting a non-undoable command
-- 3: protect integers (in Print and Delete) from being unnatural numbers, or less than 1 in Print's case

newtype Queue a = Queue (Seq a) deriving (Show, Eq)

type Output = Queue Char
type InternalString = Text

data Command = Delete Int64 | Append Text | Print Int64 | Undo deriving (Show, Eq)
data State = State {
    getInternal :: InternalString
  , getOutput :: Output
  , getHistory :: [Command]
  } deriving (Show, Eq)

enqueue = Queue . fromList

queuePush :: a -> Queue a -> Queue a
queuePush a (Queue sqnce) = Queue $ a <| sqnce

queueEmpty :: Queue a
queueEmpty = Queue empty

initialState :: State
initialState = State "" queueEmpty []

perform :: State -> Command -> State
perform s cmd@(Append str) = State (append (getInternal s) str)
                                   (getOutput s)
                                   (cmd : (getHistory s))
perform s cmd@(Delete i) = State (dropEnd i $ getInternal s)
                                 (getOutput s)
                                 (cmd : getHistory s)
perform s (Print i) = State (getInternal s)
                            ((index (getInternal s) (i - 1)) `queuePush` (getOutput s))
                            (getHistory s)
perform s Undo = State (getInternal $ performAll initialState (tail $ getHistory s))
                       (getOutput s)
                       (tail $ getHistory s)

performAll :: State -> [Command] -> State
performAll s (cmd:cmds) = performAll (perform s cmd) cmds
performAll s [] = s
