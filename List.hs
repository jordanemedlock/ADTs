
class List l where
  pushBack :: l a -> a -> l a
  pushFront :: l a -> a -> l a
  popBack :: l a -> (Maybe a, l a)
  popFront :: l a -> (Maybe a, l a)
  insert :: l a -> a -> Int -> l a
  replace :: l a -> a -> Int -> l a
  contains :: l a -> a -> Bool
  remove :: l a -> Int -> l a
  get :: l a -> Int -> Maybe a
  first :: l a -> Maybe a
  last :: l a -> Maybe a
  size :: (Num b) => l a -> b


-- add [a] to the List class
-- add Array to the List class
