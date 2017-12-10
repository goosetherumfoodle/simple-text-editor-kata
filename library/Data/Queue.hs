module Data.Queue (Queue, emptyQueue, enqueue, queuePush, printQueue) where

import Data.Sequence (Seq, empty, fromList, viewr, (<|), ViewR((:>)))

newtype Queue a = Queue (Seq a) deriving (Show, Eq)

enqueue :: [a] -> Queue a
enqueue = Queue . fromList

queuePush :: a -> Queue a -> Queue a
queuePush a (Queue sqnce) = Queue $ a <| sqnce

emptyQueue :: Queue a
emptyQueue = Queue empty

printQueue :: Show a => Queue a -> IO ()
printQueue (Queue sqnce) = iterator (viewr sqnce)
  where
    iterator (rest :> item) = print item >> iterator (viewr rest)
    iterator _ = return ()
