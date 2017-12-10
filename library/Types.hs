module Types (Command(..), State(..), Output) where

import Data.Int (Int64)
import Data.Queue (Queue)
import Data.Text.Lazy (Text)

type Output = Queue Char
type InternalString = Text

data Command = Delete Int64 | Append Text | Print Int64 | Undo deriving (Show, Eq)
data State = State {
    getInternal :: InternalString
  , getOutput :: Output
  , getHistory :: [Command]
  } deriving (Show, Eq)
