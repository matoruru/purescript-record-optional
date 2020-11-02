module Main where

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Prelude (identity, (<<<))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, class TypeEquals, RLProxy(..), RProxy)

data Required a

data Spec (spec :: Type) = Spec

class Build
  (spec :: Type) (input :: # Type) (result :: Type)
  | spec input -> result
  where
    build :: Spec spec -> Record input -> result

instance iBuild ::
  ( RL.RowToList opts opts'
  , NothingBuilder opts' () nothings
  , Row.Nub merged actual
  , Row.Union nothings reqs expected
  , TypeEquals (RProxy expected) (RProxy actual)
  , Row.Union input nothings merged
  , RL.RowToList spec spec'
  , ExtractOptionalFromDefinition spec' opts
  , ExtractRequiredFromDefinition spec' reqs
  , Row.Union nothings reqs actual
  ) => Build (Record spec) input (Record actual) where
  build _ r = merged
    where
      merged :: Record actual
      merged = Record.merge r nothings

      nothings :: Record nothings
      nothings = Builder.build builder {}

      builder :: Builder.Builder {} { | nothings }
      builder = nothingBuilder (RLProxy :: _ opts')

class ExtractRequiredFromDefinition
  (spec :: RL.RowList) (result :: # Type)
  | spec -> result

instance iExtractRequiredFromDefinitionNil ::
  ExtractRequiredFromDefinition RL.Nil ()

instance iExtractRequiredFromDefinitionIgnore ::
  ( ExtractRequiredFromDefinition tail result'
  , Row.Cons label a result' result
  ) => ExtractRequiredFromDefinition (RL.Cons label (Required a) tail) result

else instance iExtractRequiredFromDefinitionCons ::
  ( ExtractRequiredFromDefinition tail result
  ) => ExtractRequiredFromDefinition (RL.Cons label a tail) result

class ExtractOptionalFromDefinition
  (spec :: RL.RowList) (result :: # Type)
  | spec -> result

instance iExtractOptionalFromDefinitionNil ::
  ExtractOptionalFromDefinition RL.Nil ()

instance iExtractOptionalFromDefinitionIgnore ::
  ( ExtractOptionalFromDefinition tail result
  ) => ExtractOptionalFromDefinition (RL.Cons label (Required a) tail) result

else instance iExtractOptionalFromDefinitionCons ::
  ( ExtractOptionalFromDefinition tail result'
  , Row.Cons label a result' result
  ) => ExtractOptionalFromDefinition (RL.Cons label a tail) result

class NothingBuilder
  (opts :: RL.RowList) (from :: # Type) (to :: # Type)
  | opts -> from to
  where
    nothingBuilder :: RLProxy opts -> Builder.Builder { | from } { | to }

instance iNothingBuilderCons ::
  ( IsSymbol label
  , NothingBuilder tail from from'
  , Row.Lacks label from'
  , Row.Cons label (Maybe a) from' to
  ) => NothingBuilder (RL.Cons label a tail) from to where
    nothingBuilder _ =
      first <<< nothingBuilder (RLProxy :: _ tail)
      where
        label = SProxy :: _ label
        first = Builder.insert label (Nothing :: _ a)

instance iNothingBuilderNil ::
  NothingBuilder RL.Nil () () where
    nothingBuilder _ = identity
