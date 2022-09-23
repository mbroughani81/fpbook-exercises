module Ch6 where

import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, (==), (&&), discard, show, ($), Unit)

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