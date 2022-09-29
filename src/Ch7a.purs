module Ch7a where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord)
import Data.Show (class Show, show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, (==), ($), (<), (>), (<=), (>=))


data Maybe a = Nothing | Just a

-- instance eqMaybe :: Eq a => Eq (Maybe a) where
--   eq Nothing Nothing = true
--   eq (Just x) (Just y) = x == y
--   eq _ _ = false
derive instance eqMaybe :: Eq a => Eq (Maybe a)

-- instance ordMaybe :: Ord a => Ord (Maybe a) where
--   compare Nothing Nothing = EQ
--   compare Nothing _ = LT
--   compare (Just x) (Just y) = compare x y
--   compare (Just x) _ = GT
-- ordMaybe with signature Ord (Maybe a) won't work, because it expects 
-- to be an Eq instance of Maybe a, but out eqMaybe has sign Eq a => Eq (Maybe a). 
-- so it dosen't cover Eq of all possible "Maybe a"s

-- ordMaybe with signature Ord a or Eq a works, because we have Eq a => Eq (Maybe a)
-- instance!
derive instance ordMaybe :: Ord a => Ord (Maybe a)

-- instance showMaybe :: Show a => Show (Maybe a) where
--   show Nothing = "Nothing"
--   show (Just x) = "(Just " <> show x <> ")"

derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow



data Either a b = Left a | Right b
derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord(Either a b)
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow


-- greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
-- greaterThanOrEq x y = cmp == EQ || cmp == GT where cmp = compare x y
-- infixl 4 greaterThanOrEq as >= 

type MyEitherVar = Either String (Maybe Int)

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
  log "------------------"
  log $ show $ Just "abc"
  log $ show $ (Nothing :: Maybe Unit)
  log "------------------"
  log $ show $ (Left "left" :: Either _ Unit)
  log $ show $ (Right (Just 42) :: Either Unit _)
  let x = Left "left" :: MyEitherVar
      y :: MyEitherVar
      y = Right (Just 42)
  log $ show x
  log $ show y