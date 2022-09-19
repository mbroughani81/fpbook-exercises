module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, (+))


appendA :: Boolean -> Int -> String
appendA b cnt = case b of
  true -> case cnt of 
    0 -> "This is true and zero"
    _ -> "This is true and non-zero"
  false -> case cnt of
    0 -> "This is false and zero"
    _ -> "This is false and non-zero"

-- Basic useful functions

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

-- Data.List Functions

singleton :: ∀ a. a -> List a
singleton x = x : Nil
-- singleton x = Cons x Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x

-- length :: ∀ a. List a -> Int
-- length Nil = 0
-- length (_ : ys) = 1 + length ys

length :: ∀ a. List a -> Int
length l = go 0 l where
  go :: Int -> List a -> Int 
  go acc Nil = acc
  go acc (_ : xs) = go (acc + 1) xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs 

init :: ∀ a. List a -> Maybe (List a)
-- init l = go Nil l where
--   go :: List a -> List a -> Maybe (List a)
--   go _ Nil = Nothing
--   go acc (x : Nil)  = Just acc
--   go acc (x : xs) = go (x : acc) xs 
init Nil = Nothing
init l = Just $ go l where
  go Nil = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
-- uncons Nil = Nothing
-- uncons l =
--   let h = head l in
--   case h of
--     Nothing -> Nothing
--     Just x -> 
--       let t = tail l in
--       case t of
--         Nothing -> Nothing
--         Just y -> Just { head: x, tail: y }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log $ show $ Cons 3 (Cons 2 (Cons 1 Nil))
  log $ show $ 3 : 2 : 1 : Nil
  log $ show $ singleton "xyz"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log $ show $ snoc (1 : 2 : 3 : Nil) 4
  log $ show "## length ##"
  log $ show $ length (1 : 2 : 3 : 4 : 5 : Nil)
  log $ show $ (head Nil :: Maybe Unit)
  log $ show $ head (Nil :: List Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log $ show "## tail ##"
  log $ show $ (tail Nil :: Maybe (List Unit))
  log $ show $ tail ("abs" : "123" : Nil)
  log $ show "## last ##"
  log $ show $ (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" :  Nil)
  log $ show "## init ##"
  log $ show $ (init Nil :: Maybe (List Unit))
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log $ show $ uncons (1 : 2 : 3 : 4 : Nil)
  