module Main4 where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), optional)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Prim.Row as Row
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Equality (class TypeEquals)
import Type.Prelude (class IsSymbol, Proxy(..), RLProxy(..), RProxy(..))
import Type.Row (type (+))
import Type.RowList (class ListToRow) as RL
import Unsafe.Coerce (unsafeCoerce)

data Required a

type T =
  { a :: Int
  , b :: String
  , c :: Number
  , d :: String
  }

mapRecord :: forall input row xs row' merged
   . RL.RowToList row xs
  => MapRecord xs row () row'
  => Row.Union input row' merged
  => Row.Nub merged row'
  => Proxy (Record row)
  -> Record input
  -> Record row'
mapRecord _ r = ((Record.merge r $ (Builder.build builder {} :: Record row')) :: Record row')
  where
    builder = mapRecordBuilder (RLProxy :: _ xs)

class MapRecord (xs :: RL.RowList) (row :: # Type) (from :: # Type) (to :: # Type)
  | xs -> row from to where
  mapRecordBuilder :: RLProxy xs -> Builder { | from } { | to }

instance mapRecordCons ::
  ( IsSymbol name
  , MapRecord tail row from from'
  , Row.Lacks name from'
  , Row.Cons name (Maybe a) from' to
  ) => MapRecord (RL.Cons name a tail) row from to where
  mapRecordBuilder _ =
    first <<< rest
    where
      nameP = SProxy :: _ name
      rest = mapRecordBuilder (RLProxy :: _ tail)
      first = Builder.insert nameP (Nothing :: _ a)

instance mapRecordNil :: MapRecord RL.Nil row () () where
  mapRecordBuilder _ = identity

mapRecord' :: _
mapRecord' r = result
  where
    result = mapRecord (Proxy :: _ T) { a: Just 3, c: Just 3.3 }

main :: Effect Unit
main = do
  logShow $ mapRecord' {}
