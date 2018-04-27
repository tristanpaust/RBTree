module Lib where

import Control.Applicative
import qualified Data.List as LL
import Data.Function

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
ins x L = (N R L x L)
ins x (N c l y r)
  | x == y = N c l x r
  | x < y = case l of 
    (N c2 L y2 r2) -> balance((N c (N c2 (N R L x L) y2 r2) y r)) -- There is a leaf at the left so insert the new node here
    --(N c2 l2 y2 L) -> (N c (N c2 l2 y2 (N B L x L)) y r) -- I think this is wrong but for now I keep it anyways
    otherwise -> N c (ins x l) y r -- No Leaves found, hence the tree is still bigger and we need to traverse further down
  | x > y = case r of
    --(N c2 L y2 r2) -> (N c l y (N c2 (N B L x L) y2 L))
    (N c2 l2 y2 L) -> balance((N c l y (N c2 l2 y2 (N R L x L))))
    otherwise -> N c l y (ins x r)

insert :: Ord a => a -> RBTree a -> RBTree a
insert x L = (N B L x L)
insert x (N c l y r) = blackenRoot (balance(ins x (N c l y r)))

-- TODO: Balance subtrees when a new node is inserted 

fromList :: Ord a => [a] -> RBTree a
fromList [] = L
fromList (x:xs) = insert x (fromList xs)

toList :: RBTree a -> [a]
toList L = []
toList (N c l y r) = (toList l)++ [y] ++(toList r)

rootColor :: RBTree a -> Color
rootColor (N c l y r) = c

colorValue :: Color -> Int
colorValue c = 
    if c == B then
        1
    else
        0

type Path a = [(Color, a)]

paths :: RBTree a -> [Path a]
paths L = [[]]
paths (N c L y L) = [[(c,y)]]
paths (N c l y r) = do
    trees   <- [l,r]
    allPaths <- paths trees
    return ((c,y):allPaths)

test = N R L 27 L
test2 = N B (N R L 12 L) 27 (N R L 29 L)
test3 = N R (N R L 12 L) 27 (N R (N R L 28 L) 29 (N R L 32 L))
test4 = N R (N R L 12 L) 27 (N R (N R (N B L 37 L) 28 L) 29 (N R L 32 L))
{--

	27
   /  \	
12		29
       /  \
      28  32
Should give paths: 
- 27,12
- 27,29,32
- 27, 29, 28
Total: [ [(R,27),(R,12)], [(R,27),(R,29),(R,32)] [(R,27)(R,29)(R,28)] ]
--}
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
unbalanced1 = (N B (N R (N R L 6 L) 8 L) 12 L)
unbalanced2 = (N B L 6 (N R (N R L 8 L) 12 L))
unbalanced3 = (N B L 6 (N R L 8 (N R L 12 L)))
unbalanced4 = (N B (N R L 6 (N R L 8 L)) 12 L)