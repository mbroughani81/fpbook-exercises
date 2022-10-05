module Ch9 where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, class Eq, ($), (==), (&&), discard)

class Semigroup a where
  append :: a -> a -> a
infixr 5 append as <>

class Semigroup a <= Monoid a where
  mempty :: a

class Monoid a <= Group a where
  ginverse :: a -> a

data AndBool = AFalse | ATrue
derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _
instance showAndBool :: Show AndBool where
  show = genericShow

instance semigroupAndBool :: Semigroup AndBool where
  append ATrue ATrue = ATrue
  append _ _ = AFalse

instance monoidAndBool :: Monoid AndBool where
  mempty = ATrue


verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
  log "Verifying AndBool Semigroup Laws (1 test)"
  log $ show $ (AFalse <> ATrue) <> ATrue == AFalse <> (ATrue <> ATrue)

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
  log "Verifying AndBool Monoid Laws (2 tests)"
  log $ show $ (ATrue <> mempty) == ATrue && ATrue == (ATrue <> mempty)
  log $ show $ (AFalse <> mempty) == AFalse && AFalse == (AFalse <> mempty)

data OrBool = OTrue | OFalse
derive instance eqOrBool :: Eq OrBool
derive instance genericOrBool :: Generic OrBool _
instance showOrBool :: Show OrBool where
  show = genericShow

instance semigroupOrBool :: Semigroup OrBool where
  append OFalse OFalse = OFalse
  append _ _ = OTrue

instance monoidOrBool :: Monoid OrBool where
  mempty = OFalse

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
  log $ show $ (OTrue <> OFalse) <> OFalse == OTrue <> (OFalse <> OFalse) 

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do
  log $ show $ OFalse <> mempty == OFalse && mempty <> OFalse == OFalse
  log $ show $ OTrue <> mempty == OTrue && mempty <> OTrue == OTrue


data Mod4  = Zero | One | Two | Three
derive instance eqMod4 :: Eq Mod4
derive instance genericMod4 :: Generic Mod4 _
instance showMod4 :: Show Mod4 where
  show = genericShow

instance semigroupMod4 :: Semigroup Mod4 where
  append Zero x = x
  append x Zero = x

  append One One = Two
  append One Two = Three
  append One Three = Zero

  append Two One = Three
  append Two Two = Zero
  append Two Three = One

  append Three One = Zero
  append Three Two = One
  append Three Three = Two

instance monoidMod4 :: Monoid Mod4 where
  mempty = Zero

instance groupMod4 :: Group Mod4 where
  ginverse Zero = Zero
  ginverse One = Three
  ginverse Two = Two
  ginverse Three = One

verifyMod4Semigroup :: Effect Unit
verifyMod4Semigroup = do 
  log $ show $ (One <> Two) <> Three == One <> (Two <> Three) 

verifyMod4Monoid :: Effect Unit
verifyMod4Monoid = do 
  log $ show $ One <> mempty == One && mempty <> One == One

newtype First a = First (Maybe a)
derive newtype instance eqFirst :: Eq a => Eq (First a)
derive instance genericFirst :: Generic (First a) _

instance showFirst :: Show a => Show (First a) where
  show = genericShow

instance semigroupFirst :: Semigroup (First a) where
  append (First Nothing) last = last
  append first _ = first

instance monoidFirst :: Monoid (First a) where
  mempty = First Nothing

newtype Last a = Last (Maybe a)
derive newtype instance eqLast :: Eq a => Eq (Last a)
derive instance genericLast :: Generic (Last a) _

instance showLast :: Show a => Show (Last a) where
  show = genericShow

instance semigroupLast :: Semigroup (Last a) where
  append first (Last Nothing) = first
  append _ last = last

instance monoidLast :: Monoid (Last a) where
  mempty = Last Nothing

test :: Effect Unit
test = do
  log $ show $ ATrue <> ATrue
  log $ show $ ATrue <> AFalse
  log $ show $ AFalse <> ATrue
  log $ show $ "----------------------"
  log $ show $ mempty <> ATrue == ATrue
  log $ show $ mempty <> AFalse == AFalse
  log $ show $ "----------------------"
  verifyAndBoolSemigroup
  log $ show $ "----------------------"
  verifyAndBoolMonoid
  log $ show $ "----------------------"
  verifyOrBoolSemigroup
  log $ show $ "----------------------"
  verifyOrBoolMonoid
  log $ show $ "----------------------"
  verifyMod4Semigroup
  log $ show $ "----------------------"
  verifyMod4Monoid
  log $ show $ "----------------------"
  log $ show $ First Nothing <> First (Just 77)
  log $ show $ Last (Just 1) <> Last (Just 99)
