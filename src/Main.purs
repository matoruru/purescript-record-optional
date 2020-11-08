module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, Proxy(..), RLProxy(..))

data Required a

class ExtractRequiredFromDefinition
  (spec :: RL.RowList) (result :: # Type)
  | spec -> result

instance i_ExtractRequiredFromDefinitionNil ::
  ExtractRequiredFromDefinition RL.Nil ()

instance i_ExtractRequiredFromDefinitionIgnore ::
  ( ExtractRequiredFromDefinition tail result'
  , Row.Cons label a result' result
  ) => ExtractRequiredFromDefinition (RL.Cons label (Required a) tail) result

else instance i_ExtractRequiredFromDefinitionCons ::
  ( ExtractRequiredFromDefinition tail result
  ) => ExtractRequiredFromDefinition (RL.Cons label a tail) result

class ExtractOptionalFromDefinition
  (spec :: RL.RowList) (result :: # Type)
  | spec -> result

instance i_ExtractOptionalFromDefinitionNil ::
  ExtractOptionalFromDefinition RL.Nil ()

instance i_ExtractOptionalFromDefinitionIgnore ::
  ( ExtractOptionalFromDefinition tail result
  ) => ExtractOptionalFromDefinition (RL.Cons label (Required a) tail) result

else instance i_ExtractOptionalFromDefinitionCons ::
  ( ExtractOptionalFromDefinition tail result'
  , Row.Cons label a result' result
  ) => ExtractOptionalFromDefinition (RL.Cons label a tail) result

mapRecord :: forall mytype mytype' opts opts' reqs optsreqs input xs merged
   . RL.RowToList opts xs
  => MapRecord xs () opts'
  => Row.Nub merged optsreqs
  => Row.Union input opts' merged
  => RL.RowToList mytype mytype'
  => ExtractOptionalFromDefinition mytype' opts
  => ExtractRequiredFromDefinition mytype' reqs
  => Row.Union opts' reqs optsreqs
  => Proxy (Record mytype)
  -> Record input
  -> Record optsreqs
mapRecord _ r = merged
  where
    merged :: Record optsreqs
    merged = Record.merge r nothings

    nothings :: Record opts'
    nothings = Builder.build builder {}

    builder :: Builder {} { | opts' }
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

type MyType =
  ( a :: Int
  , b :: String
  , c :: Number
  , d :: String
  , e :: Required Int
  , f :: Required String
  )

type Constraints result input output =
  forall trash. Row.Union input result trash => Row.Nub trash output => Record input -> Record output

mapRecord' r = result
  where
    result = mapRecord (Proxy :: _ { | MyType }) r

main :: Effect Unit
main = do
  logShow $ mapRecord' { b: Just "Hi", e: 1, f: "Hii" }
