module Ch6 where

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.String (CodePoint)
import Data.String as StringUnicode
import Data.String.CodePoints (CodePoint)
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, (==), (&&), discard, show, ($), Unit)

class Decapitate collection element where
  decapitate :: collection -> Maybe {head :: element, tail :: collection}

instance decapitateList :: Decapitate (List a) a where
  decapitate = List.uncons

instance decapitateString :: Decapitate String Char where
  decapitate = String.uncons

genericTail 
  :: ∀ collection element
  . Decapitate collection element
  => collection 
  -> Maybe collection
genericTail xs = case (decapitate xs :: Maybe {head :: element, tail :: collection}) of
  Just { tail } -> Just tail
  Nothing -> Nothing
-- end tt
-- class Eq a where
--   eq :: a -> a -> Boolean

-- infix 4 eq as ==

data Address = Address
  { street1 :: String
  , street2 :: String
  , city :: String
  , state :: String
  , zip :: String
  }

instance eqAddress :: Eq Address where
  eq (Address a1) (Address a2) = a1 == a2

data Person = Person
  { name :: String
  , age :: Int
  , address :: Address
  }

instance eqPerson :: Eq Person where
  eq (Person p1) (Person p2) = 
    p1.name == p2.name && p1.age == p2.age && p1.address == p2.address

data SomeType = This | That | TheOther | AndYetAnother 
derive instance genericSomeType :: Generic SomeType _
instance showSomeType :: Show SomeType where
  show = genericShow

test :: Effect Unit
test = do
  log $ show $
    Person { 
      name: "a"
      , age: 10
      , address: Address {street1: "s1", street2: "s2", city: "c", state: "s", zip: "z"}} == 
      Person { 
      name: "a"
      , age: 10
      , address: Address {street1: "s1", street2: "s2", city: "c", state: "s", zip: "z"}}
  -- log $ show $
  --   (genericTail :: _ -> Maybe {head :: CodePoint, tail :: _})  "abc"