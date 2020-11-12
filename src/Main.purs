module Main where

import Prelude

import Build (class Build, build)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (logShow)
import Type (type (-*), Required, Spec(..))

type PersonSpec =
  ( name    :: String
  , age     :: Int
  , job     :: String
  )

type Person =
  { name    :: Maybe String
  , age     :: Maybe Int
  , job     :: Maybe String
  }

conversation
  :: forall input
   . Build PersonSpec input Person
  => Record input
  -> String
conversation input = name <> age <> job
  where
    person :: Person
    person = build (Spec :: _ PersonSpec) input

    name :: String
    name = "Hello, " <> fromMaybe "stranger" person.name <> ". "

    age :: String
    age = case person.age of
            Just a  -> "Oh you are " <> show a <> ", cool. "
            Nothing -> "You don't wanna tell me your age, ok. "

    job :: String
    job = case person.job of
            Just j  -> "You are a good " <> j <> "."
            Nothing -> "Let's work together."

main :: Effect Unit
main = do
  logShow $ conversation {}
