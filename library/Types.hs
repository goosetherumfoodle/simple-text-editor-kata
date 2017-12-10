module Types (Command(..), State(..), Output, InternalString) where

import Data.Int (Int64)
import Data.Queue (Queue)
import Data.Text.Lazy (Text)

-- main editor commands
-- (Int64 is the preferred index value for Data.Text)
type Output = Queue Char
type InternalString = Text
data Command = Delete Int64 | Append Text | Print Int64 | Undo deriving (Show, Eq)
data State = State {
    getInternal :: InternalString -- main string
  , getOutput :: Output -- string that will be printed
  , getHistory :: [Command] -- history of undoable commands (so not Print, or Undo)
  } deriving (Show, Eq)