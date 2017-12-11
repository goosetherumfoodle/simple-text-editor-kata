{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import qualified Test.Tasty
import Test.Tasty.Hspec
import Data.ByteString (readFile)
import Text.Trifecta (Result(..))
import Editor (perform
              , initialState
              , performAll
              )
import Data.Queue (emptyQueue, enqueue)
import Data.Dequeue (emptyDequeue, endequeue)
import Types
import qualified Parser (main)

main :: IO ()
main = do
    test <- testSpec "simple-text-editor" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  describe "Editor module" $ do
    describe "perform" $ do
      describe "appending" $ do
        context "to initial state" $ do
          it "returns correct state" $ do
            let outputState = State "horse" emptyQueue $ endequeue [Append "horse"]
            perform initialState (Append "horse") `shouldBe` outputState

        context "to existing state" $ do
          let previousState = State "initial" emptyQueue $ endequeue [Append "initial"]
          it "returns correct state" $ do
            let outputState = State "initial new" emptyQueue $ endequeue [Append " new", Append "initial"]
            perform previousState (Append " new") `shouldBe` outputState

      describe "deleting" $ do
        context "more characters than exist in the internal string" $ do
          it "doesn't error, just blanks internal string" $ do
            let previousState = State "initial state" emptyQueue $ endequeue [Append "initial state"]
                outputState = State "" emptyQueue $ endequeue [Delete 100, Append "initial state"]
            perform previousState (Delete 100) `shouldBe` outputState

        context "less characters than exist in internal string" $ do
          it "removes correct number of characters" $ do
            let previousState = State "initial state" emptyQueue $ endequeue [Append "initial state"]
                outputState = State "initial" emptyQueue $ endequeue [Delete 6, Append "initial state"]
            perform previousState (Delete 6) `shouldBe` outputState

      describe "printing" $ do
        context "with index greater than internal string's last index position" $ do
          it "just outputs the last char" $ do
            let previousState = State "initial" (enqueue "i") $ endequeue [Append "initial"]
                outputState = State "initial" (enqueue "li") $ endequeue [Append "initial"]
            perform previousState (Print 100) `shouldBe` outputState

        context "with blank internal string" $ do
          it "doesn't fail, but just doesn't modify output string" $ do
            let previousState = State "" (enqueue "a") $ endequeue [Delete 1, Append "a"]
                outputState = State "" (enqueue "a") $ endequeue [Delete 1, Append "a"]
            perform previousState (Print 1) `shouldBe` outputState

        context "with valid index" $ do
          it "conses the correct character into the output" $ do
            let previousState = State "initial" (enqueue "i") $ endequeue [Append "initial"]
                outputState = State "initial" (enqueue "ni") $ endequeue [Append "initial"]
            perform previousState (Print 2) `shouldBe` outputState

      describe "undoing" $ do
        it "pops last command from history and regenerates internal string" $ do
          let previousState = State "1" emptyQueue $ endequeue [Delete 1, Append "2", Append "1"]
              outputState = State "12" emptyQueue $ endequeue [Append "2", Append "1"]
          perform previousState Undo `shouldBe` outputState

    describe "performAll" $ do
      it "handles appends and deletes" $ do
        let outputState = (State "ac" (enqueue "c") $ endequeue [Append "c", Delete 1, Append "b", Append "a"])
            commands = endequeue [Print 2, Append "c", Delete 1, Append "b", Append "a"]
        performAll initialState commands `shouldBe` outputState

      it "handles print and undo commands" $ do
        let outputState = State "ad" (enqueue "dca") $ endequeue [Append "d", Delete 1, Append "c", Append "a"]
            commands = endequeue $ reverse [
              Append "a"
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
          let outputState = State "remaining" emptyQueue $ endequeue [Append "remaining"]
              commands = endequeue $ reverse [
                Append "a"
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
        parsed <- (Parser.main <$> readFile "data/input-sample.txt")
        case parsed of (Success commands) -> (getOutput $ performAll initialState commands) `shouldBe` expectedOutput
                       (Failure err) -> (enqueue $ show $ err) `shouldBe` expectedOutput
