module Data.Queue (Queue, emptyQueue, enqueue, queuePush) where

import Data.Sequence (Seq
                     , empty
                     , (<|)
                     , fromList)

newtype Queue a = Queue (Seq a) deriving (Show, Eq)

enqueue :: [a] -> Queue a
enqueue = Queue . fromList

queuePush :: a -> Queue a -> Queue a
queuePush a (Queue sqnce) = Queue $ a <| sqnce

emptyQueue :: Queue a
emptyQueue = Queue empty
