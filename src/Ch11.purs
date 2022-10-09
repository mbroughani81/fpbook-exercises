module Ch11 where

import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.List (List(..), (:))
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Ord (class Ord)
import Data.Semiring (class Semiring)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, otherwise, show, zero, ($), (+), (<), (<>), (<<<))

reverse :: List ~> List
-- reverse Nil = Nil
-- reverse ol = go Nil ol where
--   go rl Nil = rl
--   go rl (x : xs) = go (x : rl) xs
reverse = foldl (\rl x -> x : rl) Nil

-- max :: List a ->
max :: ∀ a. Ord a => a -> a -> a 
max a b | a < b     = b
        | otherwise = a

-- findMax :: ∀ a. Ord a => a -> List a -> a
-- findMax default Nil = default
-- findMax default (x : xs) = max x (findMax default xs)
-- findMax mx Nil = mx
-- findMax mx (x : xs) = findMax (max mx x) xs
findMax :: ∀ a. Ord a => List a -> Maybe a
-- findMax Nil = Nothing
-- findMax (x : xs) = Just (go x xs) where
--   go mx Nil = mx
--   go mx (y : ys) = go (max mx y) ys 
findMax Nil = Nothing
-- findMax (x : xs) = Just $ foldl (\mx x -> max mx x) x xs
findMax l@(first : _) = Just $ foldl max first l

foldl1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a 
foldl1 fun (x :| xs) = foldl fun x xs  

findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
-- findMaxNE (NonEmptyList (x :| xs)) = go x xs where
--   go mx Nil = mx
--   go mx (y : ys) = go (max mx y) ys
-- findMaxNE (NonEmptyList (x :| xs)) = foldl (\mx e -> max mx e) x xs 
findMaxNE (NonEmptyList xs) = foldl1 (\mx e -> max mx e) xs

sum :: ∀ f a. Foldable f => Semiring a => f a -> a
-- sum Nil = 0
-- sum (x : xs) = x + sum xs 

-- sum l = go 0 l where 
--   go curSum Nil = curSum
--   go curSum (y : ys) = go (curSum + y) ys
sum l = foldl (+) zero l

data Tree a = Leaf a | Node (Tree a) (Tree a)

toList :: ∀ a. Tree a -> List a
toList (Leaf a) = a : Nil
toList (Node x y) = toList x <> toList y

instance foldableTree :: Foldable Tree where
  foldr f acc = foldr f acc <<< toList
  foldl f acc = foldl f acc <<< toList
  foldMap f = foldMap f <<< toList 

test :: Effect Unit
test = do
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ max (-1) 99 
  log $ show $ max "aa" "z"
  log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax ("a" : "bbb" : "c" : Nil)
  log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil)) 
  log $ show $ sum (1 : 2 : 3 : Nil)
  log $ show $ sum (1.0 : 2.0 : 3.0 : Nil)
  log $ show $ sum [1, 2, 3]
  log $ show $ sum [1.0, 2.0, 3.0] 
  log $ show $ sum
    (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
  log $ show $ toList 
    (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
  