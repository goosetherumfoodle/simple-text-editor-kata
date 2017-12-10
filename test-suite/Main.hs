{-# LANGUAGE OverloadedStrings #-}

import qualified Test.Tasty
import Test.Tasty.Hspec
import Editor (perform
              , Command(..)
              , State(..)
              , initialState
              , performAll)

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
          let outputState = (State "horse" "" [Append "horse"])
          perform initialState (Append "horse") `shouldBe` outputState

      context "to existing state" $ do
        let previousState = (State "initial" "" [Append "initial"])
        it "returns correct state" $ do
          let outputState = (State "initial new" "" [Append " new", Append "initial"])
          perform previousState (Append " new") `shouldBe` outputState

    describe "deleting" $ do
      it "removes correct number of characters" $ do
        let previousState = (State "initial state" "" [Append "initial state"])
            outputState = (State "initial" "" [Delete 6, Append "initial state"])
        perform previousState (Delete 6) `shouldBe` outputState

    describe "printing" $ do
      it "conses the correct character into the output" $ do
        let previousState = (State "initial" "i" [Append "initial"])
            outputState = (State "initial" "ni" [Append "initial"])
        perform previousState (Print 2) `shouldBe` outputState

    describe "undoing" $ do
      context "with previous command an append" $ do
        it "removes appended text, and pops append command from history" $ do
          let previousState = (State "initial new" "" [Append " new", Append "initial"])
              outputState = (State "initial" "" [Append "initial"])
          perform previousState Undo `shouldBe` outputState

  describe "performAll" $ do
    it "handles appends and deletes" $ do
      let outputState = (State "ac" "c" [Append "c", Delete 1, Append "b", Append "a"])
          commands = [Append "a", Append "b", Delete 1, Append "c", Print 2]
      performAll initialState commands `shouldBe` outputState

    it "handles all commands" $ do
      let outputState = (State "ad" "dca" [Append "d", Delete 1, Append "c", Append "a"])
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
