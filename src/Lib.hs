module Lib where

import qualified Data.List as LL
import Control.Applicative
import Data.Function

data Color = R | B 
  deriving (Eq, Show)

data RBTree a = L | N Color (RBTree a) a (RBTree a)
  deriving (Show)

find :: Ord a => a -> RBTree a -> Maybe a
find x L = Nothing
find x (N c t y u)
  | x == y = Just y
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
    (N c2 L y2 L) -> if x < y2 then balance((N c (N B (N R L x L) y2 L) y r)) else balance((N c (N B L y2 (N R L x L)) y r))
    otherwise -> N c (ins x l) y r -- No Leaves found, hence the tree is still bigger and we need to traverse further down
  | x > y = case r of
    (N c2 L y2 L) -> if x < y2 then balance(N c l y (N B (N R L x L) y2 L)) else balance(N c l y (N B L y2 (N R L x L)))
    otherwise -> N c l y (ins x r)

insert :: Ord a => a -> RBTree a -> RBTree a
insert x L = (N B L x L)
insert x (N c l y r) = blackenRoot (ins x (N c l y r))

fromList :: Ord a => [a] -> RBTree a
fromList [] = L
fromList (x:xs) = insert x (fromList xs)

toList :: RBTree a -> [a]
toList L = []
toList (N c l y r) = (toList l) ++ [y] ++ (toList r)

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

{--
paths2 L = [[]]
paths (N c L x L)
paths2 (N c l x r) 
  | (N c L x L) = map (x:) (paths2 l ++ paths2 r)
  | otherwise = paths2 l
--}

data Indexed i a = Indexed (i,a)
  deriving Show

instance (Eq i) => Eq (Indexed i a) where
  (Indexed (i1,a1)) == (Indexed (i2,a2)) = i1 == i2

instance (Ord i) => Ord (Indexed i a) where
  (Indexed (i1,a1)) > (Indexed (i2,a2)) = i1 > i2
  (Indexed (i1,a1)) <= (Indexed (i2,a2)) = i1 <= i2

type RBMap i a = RBTree (Indexed i (Maybe a))

deleteAt :: Ord i => i -> RBMap i a -> RBMap i a
deleteAt i L = L
deleteAt i (N c l (Indexed (j, Just v)) r)
  | i == j = (N c l (Indexed (j,Nothing)) r)
  | i <  j = (N c (deleteAt i l) (Indexed (j, Just v)) r) 
  | i >  j = (N c l (Indexed (j, Just v)) (deleteAt i r))
{--
convertItoA :: RBMap i a -> Int
convertItoA (N c L (Indexed (j, Just v)) L) = j
--}
replaceValues :: RBMap i a -> RBTree a
replaceValues (N c L (Indexed (j, Just v)) L) = (N c L v L)
replaceValues (N c L (Indexed (j, Just v)) r) = (N c L v (replaceValues r))
replaceValues (N c l (Indexed (j, Just v)) L) = (N c (replaceValues l) v L)
replaceValues (N c l (Indexed (j, Just v)) r) = (N c (replaceValues l) v (replaceValues r))

{--
testfind i (N c l (Indexed (j, Just v)) r) = find i (replaceValues (N c l (Indexed (j, Just v)) r))
--}

findAt :: Ord i => i -> RBMap i a -> Maybe a
findAt i L = Nothing
findAt i (N c l (Indexed (j, Nothing)) r) = Nothing
findAt i (N c l (Indexed (j, Just v)) r)
  | i == j = do
    ((Indexed (j, Just v))) <- find (Indexed (j, Just v)) (N c l (Indexed (j, Just v)) r)
    return v
  | i < j = findAt i l
  | i > j = findAt i r

insertAt :: Ord i => i -> a -> RBMap i a -> RBMap i a
insertAt i a L = insert (Indexed (i, Just a)) L
insertAt i a (N c l (Indexed (j, Just v)) r) = insert (Indexed (i, Just a)) (N c l (Indexed (j, Just v)) r)
-- insertAt 1 5 L
-- -> N B L (Indexed (1,Just 5)) L
-- insertAt 5 12 maptest4
-- -> N B (N R L (Indexed (1,Nothing)) L) (Indexed (2,Just 27)) (N R (N R (N B L (Indexed (3,Just 37)) L) (Indexed (4,Just 28)) L) (Indexed (5,Just 12)) (N R L (Indexed (6,Just 32)) L))
-- insertAt 25 17 maptest4
-- -> N B (N R L (Indexed (1,Nothing)) L) (Indexed (2,Just 27)) (N R (N R (N B L (Indexed (3,Just 37)) L) (Indexed (4,Just 28)) L) (Indexed (5,Just 29)) (N B L (Indexed (6,Just 32)) (N R L (Indexed (25,Just 17)) L)))

toAssoc :: RBMap i a -> [Indexed i a]
toAssoc (N c l (Indexed (j, Nothing)) r) = []
toAssoc (N c L (Indexed (j, Just v)) L) = [(Indexed (j, v))]
toAssoc (N c l (Indexed (j, Just v)) L) = (toAssoc l) ++ [(Indexed (j, v))]
toAssoc (N c L (Indexed (j, Just v)) r) = [(Indexed (j, v))] ++ (toAssoc r)
toAssoc (N c l (Indexed (j, Just v)) r) = (toAssoc l) ++ [(Indexed (j, v))] ++ (toAssoc r)
-- toAssoc maptest4
-- [Indexed (2,27),Indexed (3,37),Indexed (4,28),Indexed (5,29),Indexed (6,32)]

fromAssoc :: Ord i => [Indexed i a] -> RBMap i a
fromAssoc [] = L
fromAssoc (Indexed(i,a):xs) = insertAt i a (fromAssoc xs)
-- fromAssoc [Indexed(1,1), Indexed(2,2), Indexed(3,3), Indexed(4,4)]
-- -> N B (N B (N B (N R L (Indexed (1,Just 1)) L) (Indexed (2,Just 2)) L) (Indexed (3,Just 3)) L) (Indexed (4,Just 4)) L


test = N R L 27 L
test2 = N B (N R L 12 L) 27 (N R L 29 L)
test3 = N R (N R L 12 L) 27 (N R (N R L 28 L) 29 (N R L 32 L))
test4 = N R (N R L 12 L) 27 (N R (N R (N B L 37 L) 28 L) 29 (N R L 32 L))

maptest = (N R L (Indexed (2, Just 12)) L)
maptest2 = (N R L (Indexed (1,Nothing)) L)
maptest3 = (N R (N B L (Indexed (1, Just 6)) L) (Indexed (2, Just 8)) (N B L (Indexed (3, Just 12)) L))
maptest4 = N R (N R L (Indexed (1, Nothing)) L) (Indexed (2, Just 27)) (N R (N R (N B L (Indexed (3, Just 37)) L) (Indexed (4, Just 28)) L) (Indexed (5, Just 29)) (N R L (Indexed (6, Just 32)) L))

{--

  27
   /  \ 
12    29
       /  \
      28  32
Should give paths: 
- 27, 12
- 27, 29, 32
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
balance1 = balance unbalanced1
unbalanced2 = (N B L 6 (N R (N R L 8 L) 12 L))
unbalanced3 = (N B L 6 (N R L 8 (N R L 12 L)))
unbalanced4 = (N B (N R L 6 (N R L 8 L)) 12 L)