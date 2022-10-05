module Ch9 where

import Data.Generic.Rep (class Generic)
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
