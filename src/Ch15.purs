module Ch15 where

import Prelude

import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Int.Bits ((.&.))
import Data.Profunctor (class Profunctor)
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