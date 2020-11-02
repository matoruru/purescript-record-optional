module Example where

import Data.Maybe (Maybe)
import Main (class Build, Required, Spec(..), build)

type Person =
  { name       :: Required String
  , age        :: Required Int
  , region     :: Required String
  , region2    :: String
  , middlename :: String
  }

type Person' =
  { name       :: String
  , age        :: Int
  , region     :: String
  , region2    :: Maybe String
  , middlename :: Maybe String
  }

createPerson :: forall input. Build Person input _ => Record input -> _
createPerson r = result
  where
    result :: Person'
    result = build (Spec :: _ Person) r
