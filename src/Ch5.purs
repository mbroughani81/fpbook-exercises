module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, (+), (-), (>=), (/=), (==), negate, type (~>))


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

-- index :: ∀ a. List a -> Int -> Maybe a
-- index l idx = if idx >= length l Nothing else 
index :: ∀ a. List a -> Int -> Maybe a
-- index Nil _ = Nothing
index Nil _ = Nothing
index (x : _) 0 = Just x
index (_ : xs) i = index xs (i - 1) 

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex pred l = go 0 l where
  go _ Nil = Nothing
  go i (x : xs) = if pred x then Just i else go (i + 1) xs  


findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
-- findLastIndex pred l = head (goodl 0 Nil l) where
--   goodl :: Int -> List Int -> List a -> List Int
--   goodl _ cur Nil = cur
--   goodl idx cur (x : xs) = if pred x then goodl (idx + 1) (idx : cur) xs else goodl (idx + 1) cur xs  
findLastIndex pred l = go Nothing 0 l where
  go fi _ Nil = fi
  go fi i (x : xs) = go (if pred x then Just i else fi) (i + 1) xs

-- reverse :: ∀ a. List a -> List a
reverse :: List ~> List 
-- reverse Nil = Nil
-- reverse (x : xs) = add x (reverse xs) where
--   add :: ∀ a. a -> List a -> List a
--   add t Nil = t : Nil
--   add t (y : ys) = y : (add t ys) 
reverse l = go Nil l where
  go rl Nil = rl
  go rl (x : xs) = go (x : rl) xs


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
  log $ show "## index ##"
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show "## !! ##"
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log $ show "## findIndex ##"
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ == 3123) Nil
  log $ show "## findLastIndex ##"
  log $ show $ findLastIndex (_ == 10) Nil
  log $ show $ findLastIndex (_ == 10) (20 : 5 : 10 : -1 : 2 : 10 : -1 : Nil)
  log $ show $ findLastIndex (_ == 3123) (11 : 12 : Nil)
  log $ show "## reverse ##"
  log $ show $ reverse (10 : 20 : 30 : 40 : 50 : -1 : Nil)
  log $ show $ reverse (Nil :: List Unit)
