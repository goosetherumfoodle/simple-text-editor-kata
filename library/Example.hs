module Example (main) where

-- TODO: protect undoable command list (in state) from getting a non-undoable command

newtype OutputString = Output String
newtype InternalString = Internal String

data Command = Delete Int | Append String | Print Int | Undo
data State = State {
    getInternalString :: InternalString
  , getOutputString :: OutputString
  , getHistory :: [Command]
  }

-- perform :: State -> Command -> State
-- perform s (Append String) =


main :: IO ()
main = pure ()
