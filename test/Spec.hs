import Test.QuickCheck
import Test.Hspec
import Control.Applicative
import qualified Data.List as LL
import Lib

main :: IO ()
main = do
  quickCheck inOrder
  quickCheck isBlackRoot
  quickCheck noRedChain
  quickCheck samePathValues
  quickCheck roundTripSort
  quickCheck findAfterInsert

-- Generate random list and create trees from it
instance (Arbitrary a, Ord a) => Arbitrary (RBTree a) where
  arbitrary = do
    a <- arbitrary
    return (fromList (LL.nub a))

-- Helper function for inOrder
checkOrder [] = True
checkOrder [x] = True
checkOrder (x:y:tl) = (x < y) && (checkOrder(y:tl))

-- Check whether rbTree is properly ordered
inOrder :: RBTree Int -> Bool
inOrder t = checkOrder (toList t)

-- Check whether rbTree has a black root
isBlackRoot :: RBTree Int -> Bool
isBlackRoot L = True
isBlackRoot (N c l v r) = c == B

-- Check whether rbTree has multiple connected red nodes in one branch
noRedChain :: RBTree Int -> Bool
noRedChain L = True
noRedChain (N R (N R _ _ _) _ _) = False
noRedChain (N R _ _ (N R _ _ _)) = False
noRedChain (N _ l x r) = (noRedChain l) && (noRedChain r)

-- Get only the colors of every path
getAllColors L = [[]]
getAllColors (N c L y L) = [[colorValue c]]
getAllColors (N c l y L) = do
  left <- [l]
  allLeft <- getAllColors left
  return ((colorValue c):allLeft)
getAllColors (N c L y r) = do 
  right <- [r]
  allRight <- getAllColors right
  return ((colorValue c):(allRight))
getAllColors (N c l y r) = do
    trees   <- [l,r]
    allPaths <- getAllColors trees
    return ((colorValue c):(allPaths))

-- Compare values in 1D summed color value list
checkPathValues [] = True
checkPathValues xs = and $ map (== head xs) (tail xs)

-- Remove the root from the lists of paths as it always is just [1]
filterRoot xs = filter (\x -> length(x) >= 2) xs

 -- Check whether all paths have the same sum, that is, all black nodes have a value of 1 and red ones have a value of 0
samePathValues :: RBTree Int -> Bool
samePathValues L = True
samePathValues t = checkPathValues (map sum ((getAllColors t)))



-- The same as the above, just with an extra insert
inOrderInsert x = inOrder . insert x
isBlackRootInsert x = isBlackRoot . insert x
noRedChainInsert x = noRedChain . insert x
samePathValuesInsert x = noRedChain . insert x

-- Make sure that a list generated from a tree that was generated from a list is the same as an ordered list
roundTripSort :: [Int] -> Bool
roundTripSort [] = True
roundTripSort (x:xs) = toList (fromList (LL.nub xs)) == LL.sort (LL.nub xs)

-- Insert element and then try to find it
findAfterInsert :: Int -> RBTree Int -> Bool
findAfterInsert x t =  find x (insert x t) == Just x


{--
findAfterInsertAt :: Int -> Int -> RBMap Int Int -> Bool
findAfterInsertAt x i t = findAt i (insertAt i x t ) == Just x

findAfterDeleteAt :: Int -> RBMap Int Int -> Bool
findAfterDeleteAt i t = findAt i (deleteAt i t) == Nothing
--}

deleteAfterInsert :: Int -> Int -> RBMap Int Int -> Bool
deleteAfterInsert i x t = toAssoc (deleteAt i (insertAt i x t)) == toAssoc (deleteAt i t)