{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import qualified Test.Tasty
import Test.Tasty.Hspec
import Data.ByteString (readFile)
import Text.Trifecta (Result(..))
import Editor (perform
              , Command(..)
              , State(..)
              , initialState
              , performAll
              )
import Data.Queue (emptyQueue, enqueue)
import Parser (parseCommandInput)

main :: IO ()
main = do
    test <- testSpec "simple-text-editor" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  describe "perform" $ do
    describe "appending" $ do
      context "to initial state" $ do
        it "returns correct state" $ do
          let outputState = (State "horse" emptyQueue [Append "horse"])
          perform initialState (Append "horse") `shouldBe` outputState

      context "to existing state" $ do
        let previousState = (State "initial" emptyQueue [Append "initial"])
        it "returns correct state" $ do
          let outputState = (State "initial new" emptyQueue [Append " new", Append "initial"])
          perform previousState (Append " new") `shouldBe` outputState

    describe "deleting" $ do
      context "more characters than exist in the internal string" $ do
        it "doesn't error, just blanks internal string" $ do
          let previousState = (State "initial state" emptyQueue [Append "initial state"])
              outputState = (State "" emptyQueue [Delete 100, Append "initial state"])
          perform previousState (Delete 100) `shouldBe` outputState

      context "less characters than exist in internal string" $ do
        it "removes correct number of characters" $ do
          let previousState = (State "initial state" emptyQueue [Append "initial state"])
              outputState = (State "initial" emptyQueue [Delete 6, Append "initial state"])
          perform previousState (Delete 6) `shouldBe` outputState

    describe "printing" $ do
      context "with index greater than internal string's last index position" $ do
        it "just outputs the last char" $ do
          let previousState = (State "initial" (enqueue "i") [Append "initial"])
              outputState = (State "initial" (enqueue "li") [Append "initial"])
          perform previousState (Print 100) `shouldBe` outputState

      context "with valid index" $ do
        it "conses the correct character into the output" $ do
          let previousState = (State "initial" (enqueue "i") [Append "initial"])
              outputState = (State "initial" (enqueue "ni") [Append "initial"])
          perform previousState (Print 2) `shouldBe` outputState

    describe "undoing" $ do
      context "with previous command an append" $ do
        it "removes appended text, and pops append command from history" $ do
          let previousState = (State "initial new" emptyQueue [Append " new", Append "initial"])
              outputState = (State "initial" emptyQueue [Append "initial"])
          perform previousState Undo `shouldBe` outputState

  describe "performAll" $ do
    it "handles appends and deletes" $ do
      let outputState = (State "ac" (enqueue "c") [Append "c", Delete 1, Append "b", Append "a"])
          commands = [Append "a", Append "b", Delete 1, Append "c", Print 2]
      performAll initialState commands `shouldBe` outputState

    it "handles print and undo commands" $ do
      let outputState = (State "ad" (enqueue "dca") [Append "d", Delete 1, Append "c", Append "a"])
          commands = [Append "a"
                     , Print 1
                     , Append "b"
                     , Undo
                     , Append "c"
                     , Print 2
                     , Delete 1
                     , Append "d"
                     , Print 2
                     ]
      performAll initialState commands `shouldBe` outputState

    context "with too many undos" $ do
      it "doesn't error, just empties command history in state" $ do
        let outputState = (State "remaining" emptyQueue [Append "remaining"])
            commands = [Append "a"
                       , Append "b"
                       , Undo
                       , Undo
                       , Append "c"
                       , Undo
                       , Undo
                       , Undo
                       , Append "remaining"
                       ]
        performAll initialState commands `shouldBe` outputState

  describe "integration" $ do
    context "parsing and executing sample file" $ do
      it "gives correct output string" $ do
        let expectedOutput = enqueue "ayc"
        parsed <- (parseCommandInput <$> readFile "data/input-sample.txt")
        case parsed of (Success commands) -> (getOutput $ performAll initialState commands) `shouldBe` expectedOutput
                       (Failure err) -> (enqueue $ show $ err) `shouldBe` expectedOutput
