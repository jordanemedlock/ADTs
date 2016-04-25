
class Map m where
  set :: m k v -> k -> v -> m k v
  get :: m k v -> k -> Maybe v
  remove :: m k v -> k -> m k v
  size :: (Num b) => m k v -> b

-- add [(k, v)] to Map class
-- add Array to Map class
data TreeMap k v = TreeMap k v TreeMap TreeMap | Nil
