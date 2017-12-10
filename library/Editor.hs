module Editor (main
               , perform
               , Command(..)
               , State(..)
               , InternalString(..)
               , OutputString(..)
               , initialState) where

-- TODO: protect undoable command list (in state) from getting a non-undoable command

type OutputString =  String
type InternalString = String

data Command = Delete Int | Append String | Print Int | Undo deriving (Show, Eq)
data State = State {
    getInternal :: InternalString
  , getOutput :: OutputString
  , getHistory :: [Command]
  } deriving (Show, Eq)

initialState = State "" "" []

perform :: State -> Command -> State
perform s cmd@(Append string) = State ((getInternal s) ++ string) ""  (cmd : (getHistory s))


main :: IO ()
main = pure ()
