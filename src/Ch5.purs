module Ch5 where

import Prelude (Unit)

import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log "hello world"
