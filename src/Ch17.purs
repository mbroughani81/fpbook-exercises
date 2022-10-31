module Ch17 

( Age(..)
, FamilyAges(..)
, FamilyAgesRow
, Validation(..)
, Either(..)
, createFamilyAges
, test
) where

import Prelude hiding((<*>), (<$>))

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
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

newtype Validation err result = Validation (Either err result)

derive instance newtypeValidation :: Newtype (Validation err result) _

derive instance functorValidation  :: Functor (Validation err)

derive newtype instance bifunctorValidation  :: Bifunctor Validation
-- instance bifunctorValidation :: Bifunctor Validation where
--   bimap f _ (Validation (Left x)) = Validation $ Left $ f x
--   bimap _ g (Validation (Right y)) = Validation $ Right $ g y

derive instance eqValidation :: (Eq err, Eq result) => Eq (Validation err result)

derive instance ordValidation :: (Ord err, Ord result) => Ord (Validation err result)

instance applyValidation :: Semigroup err => Apply (Validation err) where
  apply :: ∀ a b. Validation err (a -> b) -> Validation err a -> Validation err b
  -- apply ff v = case ff of
  --   Validation (Left es) -> case v of
  --     Validation (Left e) -> Validation $ Left $ e <> es
  --     Validation (Right _) -> Validation $ Left es    
  --   Validation (Right f) -> f <$> v
      -- Validation (Left e) -> Validation $ Left e
      -- Validation (Right r) -> Validation $ Right $ f r   
  apply (Validation (Left es)) (Validation (Left e)) = Validation (Left (es <> e))
  apply (Validation (Left es)) _ = Validation (Left es)
  apply (Validation (Right f)) x = f <$>x

derive instance genericValidation :: Generic (Validation err result) _

instance showValidation :: (Show err, Show result) => Show (Validation err result) where
  show = genericShow

newtype Age = Age Int

derive instance genericAge :: Generic Age _

instance showAge :: Show Age where
  show = genericShow

newtype FullName = FullName String

derive instance genericFullName :: Generic FullName _

instance showFullName :: Show FullName where
  show = genericShow

type FamilyAgesRow r = ( fatherAge :: Age, motherAge :: Age, childAge :: Age | r )
type FamilyNamesRow r = ( fatherName :: FullName, motherName :: FullName, childName :: FullName | r )

newtype FamilyAges = FamilyAges { | FamilyAgesRow () }

derive instance genericFamilyAges :: Generic FamilyAges _

instance showFamilyAges :: Show FamilyAges where
  show = genericShow

newtype Family = Family { | FamilyNamesRow(FamilyAgesRow ()) }

derive instance genericFamily :: Generic Family _

instance showFamily :: Show Family where
  show = genericShow

newtype UpperAge = UpperAge Int
newtype LowerAge = LowerAge Int

validateAge :: LowerAge -> UpperAge -> Age -> String -> Validation (Array String) Age
validateAge (LowerAge x1) (UpperAge x2) (Age x3) field
  | x3 > x2 = Validation $ Left [field <> " is too old"]
  | x3 < x1 = Validation $ Left [field <> " is too young"]
  | otherwise = Validation $ Right $ Age x3

createFamilyAges :: { | FamilyAgesRow() } -> Validation (Array String) FamilyAges
createFamilyAges { fatherAge, motherAge, childAge } = 
  -- (\fAge mAge cAge -> FamilyAges { fatherAge: fAge, motherAge: mAge, childAge: cAge }) 
  --   <$> validateAge (LowerAge 18) (UpperAge 100) fatherAge "father"
  --   <*> validateAge (LowerAge 18) (UpperAge 100) motherAge "mother"
  --   <*> validateAge (LowerAge 1) (UpperAge 18) childAge "child"
  FamilyAges <$> ( { fatherAge: _, motherAge: _, childAge: _ } 
    <$> validateAge (LowerAge 18) (UpperAge 100) fatherAge "father"
    <*> validateAge (LowerAge 18) (UpperAge 100) motherAge "mother"
    <*> validateAge (LowerAge 1) (UpperAge 18) childAge "child"
  )

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

  log $ show $ createFamilyAges
    { fatherAge: Age 40, motherAge: Age 30, childAge: Age 10 }
  log $ show $ createFamilyAges
    { fatherAge: Age 400, motherAge: Age 300, childAge: Age 0 }
  log $ show $ createFamilyAges
    { fatherAge: Age 4, motherAge: Age 3, childAge: Age 10 }
  log $ show $ createFamilyAges
    { fatherAge: Age 40, motherAge: Age 30, childAge: Age 100 }
  log $ show $ createFamilyAges
    { fatherAge: Age 40, motherAge: Age 3, childAge: Age 0 }
  