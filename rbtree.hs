module RedBlackTrees where

data Color = R | B 
  deriving (Eq, Show)

data RBTree a = L | N Color (RBTree a) a (RBTree a)
  deriving (Show)

find :: Ord a => a -> RBTree a -> Maybe a
find x L = Nothing
find x (N c t y u)
  | x == y       = Just y
  | otherwise = 
    if x > y then -- Right values in a balanced tree are bigger than values on the left side
        find x u
    else 
        find x t

blackenRoot :: RBTree a -> RBTree a
blackenRoot L = L
blackenRoot (N c t y u) = 
    if c == R then
        (N B t y u) 
    else
        (N c t y u)

balance :: RBTree a -> RBTree a
balance (N B (N R (N R L v3 L) v2 L) v1 L) = (N R (N B L v3 L) v2 (N B L v1 L))
balance (N B L v1 (N R (N R L v3 L) v2 L)) = (N R (N B L v1 L) v3 (N B L v2 L))
balance (N B L v1 (N R L v2 (N R L v3 L))) = (N R (N B L v1 L) v2 (N B L v3 L))
balance (N B (N R L v2 (N R L v3 L)) v1 L) = (N R (N B L v2 L) v3 (N B L v1 L))
balance a = a

ins :: Ord a => a -> RBTree a -> RBTree a
ins x (N c l y r)
  | x == y = N c l x r
  | x < y = case l of 
    (N c2 L y2 r2) -> (N c (N c2 (N B L x L) y2 r2) y r) -- There is a leaf at the left so insert the new node here
    (N c2 l2 y2 L) -> (N c (N c2 l2 y2 (N B L x L)) y r) -- There is leaf at the right so insert the new node here
    otherwise -> N c (ins x l) y r -- No Leaves found, hence the tree is still bigger and we need to traverse further down
  | x > y = case r of
    (N c2 L y2 r2) -> (N c l y (N c2 (N B L x L) y2 L))
    (N c2 l2 y2 L) -> (N c l y (N c2 l2 y2 (N B L x L)))
    otherwise -> N c l y (ins x r)

insert :: Ord a => a -> RBTree a -> RBTree a
insert x (N c l y r) = blackenRoot (ins x (N c l y r))


test = N R L 27 L
test2 = N B (N R L 12 L) 27 (N R L 29 L)
test3 = N R (N R L 12 L) 27 (N R (N R L 28 L) 29 (N R L 32 L))
test4 = N R (N R L 12 L) 27 (N R (N R (N B L 37 L) 28 L) 29 (N R L 32 L))

{-- 
All trees should have the form:
	2
   /\
  6 12
 /\ /\
L L L L

and are printed like: (N R (N B L 6 L) 2 (N B L 12 L))
unbalanced1 looks like:
	 12
    /\
   2  L
  /\ 
 6 L
/\ 
L L

unbalanced2 looks like:
	6
   /\
  L 12
    /\
   2 L
  /\
 L L

unbalanced3 looks like:
	6
   /\
  L 2
   /\
  L 12
    /\
   L L
unbalanced4 looks like:
	12
   /\
  6 L
 /\ 
L 2
 /\ 
L L
--}
-- Test cases for all four kinds of unbalanced trees
unbalanced1 = (N B (N R (N R L 6 L) 2 L) 12 L)
unbalanced2 = (N B L 6 (N R (N R L 2 L) 12 L))
unbalanced3 = (N B L 6 (N R L 2 (N R L 12 L)))
unbalanced4 = (N B (N R L 6 (N R L 2 L)) 12 L)