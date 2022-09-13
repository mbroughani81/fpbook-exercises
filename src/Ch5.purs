module Ch5 where

import Prelude (Unit)

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show)


appendA :: Boolean -> Int -> String
appendA b cnt = case b of
  true -> case cnt of 
    0 -> "This is true and zero"
    _ -> "This is true and non-zero"
  false -> case cnt of
    0 -> "This is false and zero"
    _ -> "This is false and non-zero"

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x 

flip' :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
flip' f = \x y -> f y x 

const :: ∀ a b. a -> b -> a
const a _ = a

alwaysTrue :: Int -> Boolean
alwaysTrue = const true 

test :: Effect Unit
test = do
  -- log (show (flip appendA 1 false))
  -- log (show (const 69 "sechs"))
  log (show ( alwaysTrue 2 ))
  