module Ch15 where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Int.Bits ((.&.))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.String (length)
import Effect (Effect)
import Effect.Console (log)

odd :: Int -> Boolean
odd x = x .&. 1 == 1

data Predicate a = Predicate (a -> Boolean)

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate p) x = p x 

instance contravariantPredicate :: Contravariant Predicate where
  cmap f (Predicate g) = Predicate $ g <<< f  

data Moore s a b = Moore s (s -> b) (s -> a -> s)

instance profunctorMoore :: Profunctor (Moore s) where
  -- dimap ca bd (Moore t output transition)  = Moore t (bd <<< output) (\s -> (transition s) <<< ca)
  dimap ca bd (Moore t output transition)  = Moore t (bd <<< output) (\s c -> transition s (ca c))

-- addr :: Moore Int Int Int
-- -- addr = Moore 0 (\s -> s) (\s a -> s + a)
-- addr = Moore 0 identity (+)
addr :: ∀ a. Semiring a => Moore a a a
addr = Moore zero identity (+)

runFoldL :: ∀ f s a b. Foldable f => Moore s a b -> f a -> b
runFoldL (Moore s0 extract transform) = extract <<< foldl transform s0

sizer :: Moore Int String String
-- sizer = Moore 0 (\sum -> sum) (\sum s -> sum + length s)
sizer = dimap (\s -> length s) (\x -> "Size is " <> show x) addr 

test :: Effect Unit
test = do
  log $ show $ odd 0
  log $ show $ odd 1
  log "------------------------------------" 
  log $ show $ runPredicate (Predicate odd) $ 10 
  log $ show $ runPredicate (Predicate odd) $ 11
  log "------------------------------------" 
  log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10
  log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10
  log "------------------------------------"
  log $ show $ runFoldL addr [1, 2, 3]
  log $ show $ runFoldL addr (1.0 : 2.0 : 3.0 : Nil)
  log $ show $ runFoldL sizer [ "This", "is", "the", "test" ]
  log $ show $ length <$> [ "This", "is", "the", "test" ]
  log $ show $ (\f -> f 20) <$> Just (_ * 10)
  log $ show $ (\f -> f <$> Just 20) <$> Just (_ * 10)
  log "------------------------------------"
  log $ show $ Just (_ * 10) <*> Just 20 
  log $ show $ Just (_ * 10) <*> Nothing
  log $ show $ (Nothing :: Maybe (Int -> Int)) <*> Just 20
  