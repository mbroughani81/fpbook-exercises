module Ch17 where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
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

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)

derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)

derive instance functorEither :: Functor (Either a)

derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance bifunctorEither :: Bifunctor Either where
  bimap :: ∀ a b c d. (a -> b) -> (c -> d) -> Either a c -> Either b d
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right y) = Right $ g y

instance applyEither :: Apply (Either a) where
  apply :: ∀ b c. Either a (b -> c) -> Either a b -> Either a c
  apply (Left err) _ = Left err
  apply (Right f) x = f <$> x

instance applicativeEither :: Applicative (Either a) where
  pure :: ∀ b. b -> Either a b
  pure = Right 

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
  log $ show $ (Left "error" :: Either String Unit)
  log $ show $ ((Left "error" :: Either String Unit) == (Left "grror" :: Either String Unit))
  log $ show $ ((Right 2 :: Either String Int) == (Left "22" :: Either String Int))
  log $ show $ (_ * 2) <$> (Right 20 :: Either String Int)
  log $ show $ (_ * 2) <$> (Left "str" :: Either String Int)
  log $ show $ bimap (_ * 2) (_ * 10) (Left 20 :: Either Int Int)
  log $ show $ (Right (_ * 2) :: Either Int (Int -> Int)) <*> (Right 20 :: Either Int Int)
  log $ show $ (Left 2000 :: Either Int (Int -> Int)) <*> (Right 20 :: Either Int Int)


  log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) ==
    (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
  -- LAW: Identity
  -- pure identity <*> x = x
  log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
  -- LAW: Homomorphism
  -- pure (f x) = pure f <*> pure x
  log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
  -- LAW: Interchange
  -- u <*> pure x = pure (_ $ x) <*> u
  log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> pure negate :: Either Unit Int)
