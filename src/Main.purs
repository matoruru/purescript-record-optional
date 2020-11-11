module Main where

import Prelude

import Build (class Build, build)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Type (type (-*), Required, Spec(..))
import Type.Prelude (Proxy(..))

type Person =
  ( name       :: String -* Required
  , age        :: Int    -* Required
  , region     :: String -* Required
  , region2    :: String
  , middlename :: String
  )

createPerson :: forall input. Build Person input _ => Record input -> _
createPerson r = result
  where
    result = build (Spec :: _ Person) r

main :: Effect Unit
main = do
  logShow $ createPerson { name: "", age: 3, region: "", region2: Just "" }
  pure unit
