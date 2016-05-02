{-# LANGUAGE FlexibleInstances, FlexibleContexts, RecordWildCards #-}
import Data.Array


class RAList l where
  insert :: l a -> a -> Int -> l a
  replace :: l a -> a -> Int -> l a
  contains :: l a -> a -> Bool
  remove :: l a -> Int -> l a
  get :: l a -> Int -> Maybe a
  first :: l a -> Maybe a
  last :: l a -> Maybe a
  size :: (Num b) => l a -> b


class Queue q where
  qPush :: q a -> a -> q a
  qPop :: q a -> (Maybe a, q a)

class Stack s where
  sPush :: s a -> a -> s a
  sPop :: s a -> (Maybe a, s a)

-- add [a] to the List class
-- add Array to the List class

instance Stack [] where
  sPush xs x = x : xs
  sPop [] = (Nothing, [])
  sPop xs = (Just (head xs), tail xs)

instance Queue (Array Int) where
  qPush arr x
    | null (elems arr) = array (0,0) [(0,x)]
    | otherwise = array (0,snd (bounds arr)+1)  ((snd (bounds arr)+1,x) : assocs arr) -- arr//[(i,x) | i <- [0]]
  qPop arr
    | null (elems arr) = (Nothing, arr)
    | otherwise        = (Just (arr ! 0), array (0, snd (bounds arr) - 1) [(i-1, arr ! i) | i <- [1.. snd $ bounds arr]])


data ArrayQueue e = ArrayQueue { start :: Int, end :: Int, arr :: Array Int (Maybe e) } deriving (Show)

instance Queue ArrayQueue where
  qPush (ArrayQueue start' end' arr') x = ArrayQueue {..}
    where start = start'
          end = end' + 1
          arr'' = if end' >= snd (bounds arr')
                    then array (0, snd (bounds arr') * 2) (assocs arr' ++ [(i,Nothing) | i <- [end' .. snd (bounds arr')*2]])
                    else arr'
          arr = arr''//[(end',Just x)]
  qPop (ArrayQueue start' end' arr') = (arr' ! start', ArrayQueue {..})
    where start = start' + 1
          end = end'
          arr = arr'//[(start',Nothing)]

emptyArrayQueue :: ArrayQueue e
emptyArrayQueue = ArrayQueue 0 0 (array (0,10) [(i,Nothing) | i <- [0..10]])


reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = foldl (flip (:)) [] xs
