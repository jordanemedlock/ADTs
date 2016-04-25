{-# LANGUAGE DeriveGeneric #-}
module Set where
import Control.DeepSeq
import GHC.Generics (Generic)

class Set s where
  push :: (Ord a) => s a -> a -> s a
  pop :: (Ord a) => s a -> (Maybe a, s a)
  contains :: (Ord a) => s a -> a -> Bool
  remove :: (Ord a) => s a -> a -> s a
  size :: (Num b, Ord a) => s a -> b
  empty :: s a

instance Set [] where
  push s e = if s `contains` e then s else e : s
  pop s = if null s then (Nothing, s) else (Just $ head s, tail s)
  contains s e = e `elem` s
  remove s e = filter (/=e) s
  size = fromIntegral . length
  empty = []



data BTree a = Fork a (BTree a) (BTree a) | Leaf deriving (Generic)
instance NFData a => NFData (BTree a)

instance Show a => Show (BTree a) where
  show Leaf = "Leaf"
  show (Fork a l r) = "(Fork " ++ show a ++ " " ++ show l ++ " " ++ show r ++ ")"

instance Set BTree where
  push Leaf e = Fork e Leaf Leaf
  push (Fork a l r) e
    | a < e = Fork a l (push r e)
    | a > e = Fork a (push l e) r
    | a == e = Fork a l r
  pop Leaf = (Nothing, Leaf)
  pop (Fork a l r) = (Just a, remove (Fork a l r) a)
  contains Leaf e = False
  contains (Fork a l r) e = case (compare a e) of
                             LT -> contains r e
                             GT -> contains l e
                             EQ -> True
  remove Leaf e = Leaf
  remove (Fork a l r) e
    | a < e = Fork a l (remove r e)
    | a > e = Fork a (remove l e) r
    | a == e = pushTree r l

  size Leaf = 0
  size (Fork a l r) = 1 + size l + size r
  empty = Leaf

pushTree Leaf t = t
pushTree t Leaf = t
pushTree (Fork a l r) tree@(Fork a' _ _)
  | a < a' = Fork a l (pushTree r tree)
  | a > a' =  Fork a (pushTree l tree) r
  | a == a' = Fork a l r

height Leaf = 0
height (Fork a l r) = 1 + max (height l) (height r)  

  --       10
  --     5   15
  --   3  8 13 20
  --      11 14 16 21
