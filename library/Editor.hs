{-# LANGUAGE OverloadedStrings #-}

module Editor (main
               , perform
               , Command(..)
               , State(..)
               , InternalString(..)
               , OutputString(..)
               , initialState
               , performAll) where

import Data.Text.Lazy (Text(..)
                      , dropEnd
                      , append
                      , index)
import Data.Int (Int64)
import Debug.Trace (trace)

-- TODO: 1: protect undoable command list (in state) from getting a non-undoable command
-- 3: protect integers (in Print and Delete) from being unnatural numbers, or less than 1 in Print's case
-- 4: make output list a queue

type OutputString = String
type InternalString = Text

data Command = Delete Int64 | Append Text | Print Int64 | Undo deriving (Show, Eq)
data State = State {
    getInternal :: InternalString
  , getOutput :: OutputString
  , getHistory :: [Command]
  } deriving (Show, Eq)

initialState = State "" "" []

perform :: State -> Command -> State
perform s cmd@(Append str) = State (append (getInternal s) str)
                                   (getOutput s)
                                   (cmd : (getHistory s))
perform s cmd@(Delete i) = State (dropEnd i $ getInternal s)
                                 (getOutput s)
                                 (cmd : getHistory s)
perform s (Print i) = State (getInternal s)
                                ((index (getInternal s) (i - 1)) : (getOutput s))
                                (getHistory s)
perform s Undo = State (getInternal $ performAll (State "" "" []) (tail $ getHistory s))
                       (getOutput s)
                       (tail $ getHistory s)

performAll :: State -> [Command] -> State
performAll s (cmd:cmds) = performAll (perform s cmd) cmds
performAll s [] = s

main :: IO ()
main = pure ()
