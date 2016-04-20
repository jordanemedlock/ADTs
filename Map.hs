
class Map m where
  set :: m k v -> k -> v -> m k v
  get :: m k v -> k -> Maybe v
  remove :: m k v -> k -> m k v
