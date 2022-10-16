module Ch13 where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, discard, identity, show, ($), (*), (/), (<<<), (<>), (==))

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$> 

class Bifunctor f where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d

lmap :: ∀ a b c f. Bifunctor f => (a -> b) -> f a c -> f b c
lmap f = bimap f identity

rmap :: ∀ a b c f. Bifunctor f => (b -> c) -> f a b -> f a c
rmap f = bimap identity f

data Maybe a = Just a | Nothing

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just (f x)

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance functorEither :: Functor (Either a) where
  map _ (Left err) = Left err 
  map f (Right x) = Right $ f x  

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right x) = Right $ g x

data Tuple a b = Tuple a b

derive instance genericTuple :: Generic (Tuple a b) _

instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show = genericShow

instance functorTuple :: Functor (Tuple a) where
  map f (Tuple a b) = Tuple a (f b)

instance bifunctorTuple :: Bifunctor Tuple where
  bimap f g (Tuple x y) = Tuple (f x) (g y)

data Threeple a b c = Threeple a b c

derive instance genericThreeple :: Generic (Threeple a b c) _

instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow

instance functorThreeple :: Functor (Threeple a b) where
  map f (Threeple x y z) = Threeple x y (f z)

instance bifunctorThreeple :: Bifunctor (Threeple a) where
  bimap f g (Threeple x y z) = Threeple x (f y) (g z)

derive instance eqMaybe :: Eq a => Eq (Maybe a)

test :: Effect Unit
test = do
  log $ show $ (_ / 2) <$> Just 10
  log $ show $ (_ / 2) <$> Nothing
  -- log $ show $ (Right 10 :: Either Int Int)

  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
  log $ show $ (_ / 2) <$> Left "error reason"

  log $ show $ (_ / 2) <$> Tuple 10 20 
  log $ show $ (_ / 2) <$> Threeple 10 20 40 
  log $ show $ "Maybe Identity for Nothing: "
    <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))
  log $ show $ "Maybe Identity for Just: "
    <> show ((identity <$> Just [1, 2]) == Just [1, 2]) 
  let g x = x * 2
      f x = x * 3
  log $ show $ "Maybe Composition for Nothing: "
    <> show (map (g <<< f) Nothing == (map g <<< map f) Nothing)
  log $ show $ "Maybe Composition for Nothing: "
    <> show (map (g <<< f) (Just 10) == (map g <<< map f) (Just 10))
  
  log $ show $ rmap (_ * 2) $ Left "error reason"
  log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _)
  log $ show $ lmap toUpper $ (Left "error reason" :: Either _ Unit)
  log $ show $ lmap toUpper $ Right 10

  log $ show $ rmap (_ * 2) $ Tuple 80 40 
  log $ show $ lmap (_ / 2) $ Tuple 80 40 
  log $ show $ bimap (_ / 2) (_ * 2) $ Tuple 80 40 

  log $ show $ rmap (_ * 2) $ Threeple 99 80 40
  log $ show $ lmap (_ / 2) $ Threeple 99 80 40
  log $ show $ bimap (_ / 2) (_ * 2) $ Threeple 99 80 40 