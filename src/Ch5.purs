module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, otherwise, show, (*), (+), (-), (/=), (<), (<<<), (>>>), (<=), (==), (>), (>=))


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


concat :: ∀ a. List (List a)  -> List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

filter :: ∀ a. (a -> Boolean) -> List a -> List a
-- filter _ Nil = Nil
-- filter pred (x : xs) = if pred x then (x : filter pred xs) else filter pred xs 
-- filter pred l = reverse $ go Nil l where
--   go nl Nil = nl
--   go nl (x : xs) = if pred x then go (x : nl) xs else go nl xs
filter pred = reverse <<< go Nil where
  go nl Nil = nl
  go nl (x : xs) = if pred x then go (x : nl) xs else go nl xs

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (x : xs) = case x of
  Just y -> y : catMaybes xs
  Nothing -> catMaybes xs

range :: Int -> Int -> List Int
-- range x y
--   | (x == y) = (x : Nil)
--   | otherwise = 
--     if x < y then (x : range (x + 1) y) else (x : range (x - 1) y)
-- range x y
--   | (x == y) = (x : Nil)
--   | x < y = (x : range (x + 1) y)
--   | otherwise = (x : range (x - 1) y)
-- range start end = go (if start < end then 1 else (-1)) start where
--   go step start' | start' == end  = singleton start'
--                  | otherwise      = start' : go step (start' + step)
range start end = go start where
  step = if start < end then 1 else (-1)  
  go start' | start' == end = singleton start'
            | otherwise = start' : go (start' + step)

-- data Tuple a b = Tuple a b
checkWhere :: Int -> Int -> Tuple Int Int
checkWhere x y = Tuple summul mul where
   summul = x + y + mul
   mul = x * y

take :: ∀ a. Int -> List a -> List a
-- take _ Nil = Nil
-- take cnt (x : xs) | cnt <= 0  = Nil
--                   | otherwise = x : take (cnt - 1) xs
take cnt = reverse <<< go cnt Nil where
  go _ nl Nil = nl
  go cnt' nl (x : xs) | cnt' <= 0 = nl
                      | otherwise = go (cnt' - 1) (x : nl) xs

drop :: ∀ a. Int -> List a -> List a
drop _ Nil = Nil
drop cnt (x : xs) | cnt <= 0  = (x : xs) 
                  | otherwise = drop (cnt - 1) xs 

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile pred (x : xs) = if pred x then x : takeWhile pred xs else Nil  

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile pred l@(x : xs) = if pred x then dropWhile pred xs else l

takeEnd :: ∀ a. Int -> List a -> List a
-- takeEnd n = reverse <<< take n <<< reverse
-- takeEnd n l = (go l).curList where
--   go :: List a -> { curList :: List a, curListLength :: Int }   
--   go Nil = { curList: Nil, curListLength: 0 }
--   go (x : xs) = let g = go xs in
--     if g.curListLength < n then
--       { curList: x : g.curList, curListLength: g.curListLength + 1 }
--     else
--       g
takeEnd n = go >>> snd where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs
    # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then x : nl else nl

dropEnd :: ∀ a. Int -> List a -> List a
dropEnd n = go >>> snd where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs
    # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then Nil else x : nl

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip l1 l2 = reverse $ go Nil l1 l2 where
  go cur _ Nil = cur
  go cur Nil _ = cur
  go cur (x : xs) (y : ys) = go (Tuple x y : cur) xs ys 

unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple x1 x2 : xs) = unzip xs
  # \(Tuple l1 l2) -> Tuple (x1 : l1) (x2 : l2)

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
  log $ show "## concat ##"
  log $ show $ 
    concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
  log $ show "## filter ##"
  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : -10 : Nil)
  log $ show "## catMaybes ##"
  log $ show $ 
    catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  log $ show "## range ##"
  log $ show $ range 1 10 
  log $ show $ range 3 (-3) 
  log $ show "## checkWhere (test using where variables in other where vars definition) ##"
  log $ show $ let x = checkWhere 5 5 in case x of
    Tuple a b -> a
  log $ show $ let x = checkWhere 5 5 in case x of
    Tuple a b -> b
  log $ show "## take ##"
  log $ show $ take 5 (12 : 13 : 14 : Nil)
  log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)
  log $ show "## drop ##"
  log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil) 
  log $ show $ drop 10 (Nil :: List Unit)
  log $ show "## takeWhile ##"
  log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil) 
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
  log $ show "## dropWhile ##"
  log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil) 
  log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil)
  log $ show "## takeEnd ##"
  log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ takeEnd 10 (1 : Nil)
  log $ show "## dropEnd ##"
  log $ show $ dropEnd 4 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ dropEnd 10 (1 : Nil) 
  log $ show "## zip ##"
  log $ show $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil)
  log $ show $ zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil)
  log $ show $ zip (Nil :: List Unit) (1 : 2 : Nil) 
  log $ show "## unzip ##"
  log $ show $ unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil) 
  log $ show $ unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil) 
  log $ show $ unzip (Nil :: List (Tuple Unit Unit)) 