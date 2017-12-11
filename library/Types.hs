{-# LANGUAGE OverloadedStrings #-}
module Types (Command(..), State(..), Output, InternalString, History) where

import Data.Int (Int64)
import Data.Dequeue (Dequeue)
import Data.Text.Lazy (Text)

type Output = Dequeue Char
type InternalString = Text
type History = Dequeue Command

-- Commands to be performed by editor
-- (Int64 is the preferred index value for Data.Text)
data Command = Delete Int64 | Append Text | Print Int64 | Undo deriving (Show, Eq)

-- the state that the editor will have between commands.
-- (we use dequeues for command history and output, because both need to be
-- written at one end but read from the other)
data State = State {
    getInternal :: InternalString -- string being edited
  , getOutput :: Output -- stored output characters
  , getHistory :: History -- history of undoable commands (so not Print, or Undo)
  } deriving (Show, Eq)
