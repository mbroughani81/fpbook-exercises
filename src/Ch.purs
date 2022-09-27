module Ch where

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)
import Prelude ((&&), discard, show, ($), Unit, (+), (-))


fold :: ∀ a b. List a -> b -> (a -> b -> b) -> b
fold Nil b _ = b
fold (x : xs) b f = f x (fold xs b f)

data AccountId = AccountId Int
data Time = Time Int 

getWealth :: AccountId -> Int
getWealth x = case x of
  AccountId 1 -> 1
  AccountId 2 -> 1
  _ -> 0 

data Event = Withdraw AccountId Int Time | Deposit AccountId Int Time

read :: AccountId -> List Event
read _ = (Withdraw (AccountId 0) 1 (Time 111) : Deposit (AccountId 0) 10 (Time 111) : Nil)

process :: AccountId -> Int
process accountId = go (read accountId) where
  go :: List Event -> Int
  go events = fold events 0 f where
    f :: Event -> Int -> Int
    f = \event current -> case event of
      Withdraw _ x _  -> current - x
      Deposit _ x _ -> current + x

test :: Effect Unit
test = do
  log $ show $ let ll = (AccountId 1 : AccountId 2 : AccountId 3 : AccountId 1 : Nil) in
    fold ll 0  (\a b -> b + getWealth a)
  
