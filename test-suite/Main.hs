import qualified Test.Tasty
import Test.Tasty.Hspec
import Editor (perform
              , Command(..)
              , State(..)
              , initialState)

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
