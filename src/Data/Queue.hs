module Data.Queue (Queue, empty, isEmpty, enqueue, dequeue, fromList) where

data Queue a = Queue [a] [a]

instance Eq a => Eq (Queue a) where
  (Queue a b) == (Queue c d) = (a <> reverse b) == (c <> reverse d)

instance Show a => Show (Queue a) where
  show (Queue a b) = "Queue " ++ show (a <> reverse b)

empty :: Queue a
empty = Queue [] []

fromList :: [a] -> Queue a
fromList a = Queue a []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

enqueue :: a -> Queue a -> Queue a
enqueue a (Queue q p) = Queue q (a : p)

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue (Queue [] []) = (Nothing, empty)
dequeue (Queue (a : q) p) = (return a, Queue q p)
dequeue (Queue [] p) = dequeue (Queue (reverse p) [])
