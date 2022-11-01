module Parser where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)

type ParserState a = Tuple String a

type ParserFunction e a = ParserError e => String -> Either e (ParserState a)

class ParserError (e :: Type) -- What is e ??

newtype Parser e a = Parser (ParserFunction e a)

instance functorParser :: Functor (Parser e) where
  map :: ∀ a b. (a -> b) -> Parser e a -> Parser e b
  map f (Parser g) = Parser \s -> map f <$> (g s)

instance applyParser :: Apply (Parser e) where
  apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
  apply (Parser f) (Parser g) = Parser \s -> case f s of
    Left err -> Left err
    Right (Tuple s1 h) -> case g s1 of
      Left err -> Left err
      Right (Tuple s2 x) -> Right $ Tuple s2 (h x)

instance applicativeParser :: Applicative (Parser e) where
  pure :: ∀ a. a -> Parser e a
  -- pure x = Parser \s -> Right $ Tuple s x 
  pure x = Parser \s -> pure $ Tuple s x

parse :: ∀ a e. ParserError e => Parser e a -> ParserFunction e a
parse (Parser f) = f

test :: Effect Unit
test = do
  log "placeholder"