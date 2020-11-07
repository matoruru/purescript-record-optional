module Main5 where

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

class ExtractOptionalFromDefinition
  (spec :: RL.RowList) (result :: # Type)
  | spec -> result

instance _ExtractOptionalFromDefinitionNil ::
  ExtractOptionalFromDefinition RL.Nil ()

instance _ExtractOptionalFromDefinitionIgnore ::
  ( ExtractOptionalFromDefinition tail result
  ) => ExtractOptionalFromDefinition (RL.Cons label (Required a) tail) result

else instance _ExtractOptionalFromDefinitionCons ::
  ( ExtractOptionalFromDefinition tail result'
  , Row.Cons label a result' result
  ) => ExtractOptionalFromDefinition (RL.Cons label a tail) result

mapRecord :: forall mytype mytype' opts opts' input whole xs merged
   . RL.RowToList opts xs
  => MapRecord xs () opts'
  => Row.Union input opts' merged
  => Row.Nub merged whole

  => RL.RowToList mytype mytype'
  => ExtractOptionalFromDefinition mytype' opts

  => Proxy (Record mytype)
  -> RProxy whole
  -> Record input
  -> Record whole
mapRecord _ _ r = ((Record.merge r $ (Builder.build builder {} :: Record opts')) :: Record whole)
  where
    --mappedJust = mapJust r (RLProxy :: _ xs)
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

type MyType =
  { a :: Int
  , b :: String
  , c :: Number
  , d :: String
  , e :: Required Int
  , f :: Required String
  }

mapRecord' :: forall input. Constraints TMR input _
mapRecord' r = result
  where
    result = mapRecord (Proxy :: _ MyType) (RProxy :: _ TMRW) r

main :: Effect Unit
main = do
  logShow $ mapRecord' { a: Just 3, e: 2, f: "" }


class A (list :: RL.RowList) (result :: # Type)
  | list -> result

instance i_ANil :: A RL.Nil ()

instance i_AIgnore ::
  ( A tail result
  ) => A (RL.Cons label (Required a) tail) result

else instance i_ACons ::
  ( A tail result'
  , Row.Cons label a result' result
  ) => A (RL.Cons label a tail) result

a
  :: forall row row' result
   . RL.RowToList row row'
  => A row' result
  => Proxy (Record row)
  -> Record result
  -> Unit
a _ _ = unit

a' :: _
a' = a (Proxy :: _ MyType)
