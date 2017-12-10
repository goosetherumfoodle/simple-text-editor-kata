{-# LANGUAGE OverloadedStrings #-}

module Editor (main
               , perform
               , Command(..)
               , State(..)
               , InternalString(..)
               , OutputString(..)
               , initialState) where


import Data.Text.Lazy (Text(..)
                      , dropEnd
                      , append
                      , index)
import Data.Int (Int64)

-- TODO: 1: protect undoable command list (in state) from getting a non-undoable command
-- 2: text string type
-- 3: protect integers from being unnatural numbers

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
perform s cmd@(Append str) = State (append (getInternal s) str) (getOutput s) (cmd : (getHistory s))
perform s cmd@(Delete i) = State (dropEnd i (getInternal s)) (getOutput s) (cmd : (getHistory s))
perform s cmd@(Print i) = State (getInternal s) ((index (getInternal s) (baseZero i)) : (getOutput s)) (getHistory s)
  where
    baseZero i = i - 1


main :: IO ()
main = pure ()
