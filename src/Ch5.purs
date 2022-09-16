module Ch5 where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show)


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

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as #

test :: Effect Unit
test = do
  -- log (show (flip appendA 1 false))
  -- log (show (const 69 "sechs"))
  -- log (show ( ( apply appendA false ) 10 ))
  -- log (show ( let ff = appendA false in ff 10 ))
  -- log $ show $ ( apply appendA false ) 10
  -- log ( show $ ( apply appendA false ) 10 )
  -- log $ show $ apply appendA false  10
  log $ show $ flip const 1 2
  flip const 1 2 # show # log