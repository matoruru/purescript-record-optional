module Example where

import Prelude

import Data.Maybe (Maybe(..))
import Main (class Build, build)
import Type (type (-*), Required, Spec(..))
import Type.Prelude (Proxy(..))

type Person =
  { name       :: String -* Required
  , age        :: Int    -* Required
  , region     :: String -* Required
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
