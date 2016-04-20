
class Set s where
  add :: s a -> a -> s a
  contains :: s a -> a -> Bool
  remove :: s a -> a -> s a
