module Ch17 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

data Maybe a = Just a | Nothing

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map :: ∀ a b. (a -> b) -> Maybe a -> Maybe b
  map f (Just x) = Just $ f x
  map _ _ = Nothing

infixl 4 map as <$>

instance applyMaybe :: Apply Maybe where
  apply :: ∀ a b. Maybe (a -> b) -> Maybe a -> Maybe b 
  apply Nothing _ = Nothing
  apply (Just f) x = f <$> x

infixl 4 apply as <*>

instance applicativeMaybe :: Applicative Maybe where
  pure :: ∀ a. a -> Maybe a
  pure = Just

test :: Effect Unit
test = do
  log "placeholder"
  log $ show $ (_ * 2) <$> Just 20
  log $ show $ Just (_ * 2) <*> Just 20
  log $ show $ Just (_ * 2) <*> Nothing
  log $ show $ (Nothing :: Maybe (Int -> Int)) <*> Just 20
  log $ show $ (Nothing :: Maybe (Int -> Int)) <*> Nothing
  log $ show $ (\x y z -> x + y + z) <$> Just 30 <*> Just 20 <*> Just 10
  log $ show $ (\x y z -> x + y + z) <$> Just 30 <*> Nothing <*> Just 10
  log $ show $ "----------------------"
  log $ show $ (+) <$> Just 21 <*> Just 21
  log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)
  log $ show $ pure (+) <*> Just 17 <*> Just 25 
