module Ch7a where

import Data.Eq (class Eq)
import Data.Ord (class Ord, Ordering(..), compare)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, discard, (==), ($), (<), (>), (<=), (||))

data Maybe a = Nothing | Just a

instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq Nothing Nothing = true
  eq (Just x) (Just y) = x == y
  eq _ _ = false

instance ordMaybe :: Ord a => Ord (Maybe a) where
  compare Nothing Nothing = EQ
  compare Nothing _ = LT
  compare (Just x) (Just y) = compare x y
  compare (Just x) _ = GT
-- ordMaybe with signature Ord (Maybe a) won't work, because it expects 
-- to be an Eq instance of Maybe a, but out eqMaybe has sign Eq a => Eq (Maybe a). 
-- so it dosen't cover Eq of all possible "Maybe a"s

-- ordMaybe with signature Ord a or Eq a works, because we have Eq a => Eq (Maybe a)
-- instance!  

greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq x y = cmp == EQ || cmp == GT where cmp = compare x y
infixl 4 greaterThanOrEq as >= 

test :: Effect Unit
test = do
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit) 
  log "------------------"
  log $ show $ Just 1 < Just 5 
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 5 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing 

