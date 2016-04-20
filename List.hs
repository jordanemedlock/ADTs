
class List l where
  pushBack :: l a -> a -> l a
  pushFront :: l a -> a -> l a
  insert :: l a -> a -> Int -> l a
  replace :: l a -> a -> Int -> l a
  contains :: l a -> a -> Bool
  remove :: l a -> Int -> l a
  get :: l a -> Int -> Maybe a
