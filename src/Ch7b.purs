module Ch7b where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString) as FromIntToString
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (split, Pattern(..))
import Effect (Effect)
import Effect.Console (log)

newtype CSV = CSV String
class ToCSV a where
  toCSV :: a -> CSV

newtype FullName = FullName String
newtype Age = Age Int
data Occupation = Doctor | Dentist | Lawyer | Unemployed
data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

-- derive instance newtypeFullName :: Newtype FullName _
-- derive newtype instance showFullName :: Show FullName
instance showFullName :: Show FullName where
  show (FullName name) = name

derive instance newtypeAge :: Newtype Age _
derive newtype instance showAge :: Show Age

derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
  show = genericShow

instance toCSVPerson :: ToCSV Person where
  toCSV (Person {name, age, occupation}) =
    CSV $ show name <> "," <> show age <> "," <> show occupation
derive instance eqCSV :: Eq CSV

class FromString a where
  fromString :: String -> Maybe a

class FromCSV a where
  fromCSV :: CSV -> Maybe a

instance fromStringFullName :: FromString FullName where
  fromString s = Just (FullName s)

instance fromStringAge :: FromString Age where
  fromString s = case FromIntToString.fromString s of
    Just x -> Just (Age x)
    Nothing -> Nothing

instance fromStringOccupation :: FromString Occupation where
  fromString s = case s of 
    "Doctor" -> Just Doctor
    "Dentist" -> Just Dentist
    "Lawyer" -> Just Lawyer
    "Unemployed" -> Just Unemployed
    _ -> Nothing

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV s) = case split (Pattern ",") s of
    -- interesting thing: 
    --  [name, age, occupation] -> case { name: fromString name, age: fromString age, occupation: fromString occupation} of
    --   {name: Just f, age: Just a, occupation: Just o} -> Nothing
    -- This will return error (i think because it can not infer the name, age and occupation from rest of the code)
    [name, age, occupation] -> case { name: fromString name, age: fromString age, occupation: fromString occupation} of
      {name: Just f, age: Just a, occupation: Just o} -> Just (Person {name: f, age: a, occupation: o})
      _ -> Nothing
    _ -> Nothing 
 

test :: Effect Unit
test = do
  log $ show $ CSV "mb,18,Unemployed" == toCSV (Person { name: FullName "mb", age: Age 18, occupation: Unemployed })
  -- log $ show $ let CSV x = toCSV (Person { name: FullName "mb", age: Age 18, occupation: Unemployed }) in 
  --   x
  log $ show $ let x = (fromCSV :: CSV -> Maybe Person) (CSV "mb,20,Unemployed") in case x of 
    Just xx -> "gg"
    _ -> "Nothing"