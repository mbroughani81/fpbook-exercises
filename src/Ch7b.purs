module Ch7b where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
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

test :: Effect Unit
test = do
  log $ show $ CSV "mb,18,Unemployed" == toCSV (Person { name: FullName "mb", age: Age 18, occupation: Unemployed })
  -- log $ show $ let CSV x = toCSV (Person { name: FullName "mb", age: Age 18, occupation: Unemployed }) in 
  --   x