module RedBlackTrees where

data Color = R | B 
  deriving (Eq, Show)

data RBTree a = L | N Color (RBTree a) a (RBTree a)
  deriving (Show)

find :: Ord a => a -> RBTree a -> Maybe a
find x L = Nothing
find x (N c t y u)
  | x == y       = Just y
  | otherwise = Nothing