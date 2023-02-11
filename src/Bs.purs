module Bs where

import Prelude
import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)


oddTest :: Int -> Maybe Int
oddTest x = if x .&. 1 == 1 then Just x else Nothing
greaterThanTest :: Int -> Int -> Maybe Int
greaterThanTest min x = if x > min then Just x else Nothing
lessThanTest :: Int -> Int -> Maybe Int
lessThanTest max x = if x < max then Just x else Nothing

gauntlet' :: Int -> Maybe Int
gauntlet' = oddTest >=> greaterThanTest 10 >=> lessThanTest 20

gauntlet :: Int -> Maybe Int
-- gauntlet = oddTest >=> pure <<< (_ + 1) >=> greaterThanTest 10 >=> lessThanTest 20
gauntlet x = pure x >>= 
oddTest 
  -- >>= 
  -- \o -> pure (o + 1) >>=
  -- \y -> greaterThanTest 10 y >>=
  -- \z -> lessThanTest 20 z

test :: Effect Unit
test = do
  log $ show $ gauntlet 14
  log $ show $ gauntlet 1
  log $ show $ gauntlet 93
  log $ show $ gauntlet 17
