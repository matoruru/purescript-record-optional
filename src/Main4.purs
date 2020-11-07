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

type TMR =
  ( a :: Maybe Int
  , b :: Maybe String
  , c :: Maybe Number
  , d :: Maybe String
  )

type TMRW =
  ( a :: Maybe Int
  , b :: Maybe String
  , c :: Maybe Number
  , d :: Maybe String
  , e :: Int
  , f :: String
  )

mapRecord :: forall input row whole xs row' merged
   . RL.RowToList row xs
  => MapRecord xs () row'
  => Row.Union input row' merged
  => Row.Nub merged whole
  => Proxy (Record row)
  -> RProxy whole
  -> Record input
  -> Record whole
mapRecord _ _ r = ((Record.merge r $ (Builder.build builder {} :: Record row')) :: Record whole)
  where
    builder = mapRecordBuilder (RLProxy :: _ xs)

class MapRecord (xs :: RL.RowList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  mapRecordBuilder :: RLProxy xs -> Builder { | from } { | to }

instance mapRecordCons ::
  ( IsSymbol name
  , MapRecord tail from from'
  , Row.Lacks name from'
  , Row.Cons name (Maybe a) from' to
  ) => MapRecord (RL.Cons name a tail) from to where
  mapRecordBuilder _ =
    first <<< rest
    where
      nameP = SProxy :: _ name
      rest = mapRecordBuilder (RLProxy :: _ tail)
      first = Builder.insert nameP (Nothing :: _ a)

instance mapRecordNil :: MapRecord RL.Nil () () where
  mapRecordBuilder _ = identity

type Constraints result input output =
  forall trash. Row.Union input result trash => Row.Nub trash output => Record input -> Record output

mapRecord' :: forall input. Constraints TMR input _
mapRecord' r = result
  where
    result = mapRecord (Proxy :: _ T) (RProxy :: _ TMRW) r

main :: Effect Unit
main = do
  logShow $ mapRecord' { a: Just 3, e: 2, f: "" }


