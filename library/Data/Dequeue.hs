module Data.Dequeue (Dequeue
                    , pushLeft
                    , dropLeft
                    , popRight
                    , endequeue
                    , emptyDequeue
                    ) where

import Data.Sequence (ViewR((:>))
                     , ViewL((:<))
                     , Seq
                     , fromList
                     , empty
                     , viewr
                     , viewl
                     , (<|)
                     )

newtype Dequeue a = Dequeue { runDequeue :: (Seq a) } deriving (Show, Eq)

pushLeft :: a -> Dequeue a -> Dequeue a
pushLeft a (Dequeue sqnce) = Dequeue $ a <| sqnce

dropLeft :: Dequeue a -> Dequeue a
dropLeft = Dequeue . dropLeft' . viewl . runDequeue
  where
    dropLeft' :: ViewL a -> Seq a
    dropLeft' (_ :< sqnce) = sqnce
    dropLeft' _ = empty

popRight :: Dequeue a -> Maybe (Dequeue a, a)
popRight = popRight' . viewr . runDequeue
  where
    popRight' :: ViewR a -> Maybe (Dequeue a, a)
    popRight' (deq :> a) = Just (Dequeue deq, a)
    popRight' _ = Nothing

endequeue :: [a] -> Dequeue a
endequeue = Dequeue . fromList

emptyDequeue :: Dequeue a
emptyDequeue = Dequeue empty
