import Test.QuickCheck
import Test.Hspec
import Control.Applicative
import Lib

main :: IO ()
main = do
  quickCheck inOrder
  quickCheck isBlackRoot
  quickCheck noRedChain
  --quickCheck samePathValues


--arbitrary :: (Arbitrary a, Ord a) => Gen (RBTree a)
instance (Arbitrary a, Ord a) => Arbitrary (RBTree a) where
  arbitrary = do
    a <- arbitrary
    return (fromList a)


isBlackRoot :: RBTree Int -> Bool
isBlackRoot L = True
isBlackRoot (N c l v r) = c == B

noRedChain :: RBTree Int -> Bool
noRedChain L = True
noRedChain (N R (N R _ _ _) _ _) = False
noRedChain (N R _ _ (N R _ _ _)) = False
noRedChain (N _ l x r) = (noRedChain l) && (noRedChain r)

 -- Check for ordering violations:
checkOrder [] = True
checkOrder [x] = True
checkOrder (x:y:tl) = (x < y) && (checkOrder(y:tl))

inOrder :: RBTree Int -> Bool
inOrder t = checkOrder (toList t) 


{--
samePathValues :: RBTree Int -> Bool
samePathValues
--}