module Main where

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Prelude (identity, (<<<))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder as Builder
import Type as T
import Type.Prelude (class IsSymbol, class TypeEquals, RLProxy(..), RProxy)

foreign import kind Attr

foreign import data Required :: Attr
foreign import data Optional :: Attr

class Build
  (spec :: Type) (input :: # Type) (result :: Type)
  | spec -> result
  where
    build :: T.Spec spec -> Record input -> result

instance iBuild ::
  ( RL.RowToList opts opts'
  , NothingBuilder opts' () nothings
  , Row.Nub merged actual
  , Row.Union nothings reqs expected
  , TypeEquals (RProxy expected) (RProxy actual)
  , Row.Union input nothings merged
  , RL.RowToList spec spec'
  , ExtractFromSpec Optional spec' opts
  , ExtractFromSpec Required spec' reqs
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

class ExtractFromSpec
  (attr :: Attr) (spec :: RL.RowList) (result :: # Type)
  | attr spec -> result

instance iExtractRequiredFromSpecNil ::
  ExtractFromSpec Required RL.Nil ()

instance iExtractRequiredFromSpecIgnore ::
  ( ExtractFromSpec Required tail result'
  , Row.Cons label a result' result
  ) => ExtractFromSpec Required (RL.Cons label (T.Required a) tail) result

else instance iExtractRequiredFromSpecCons ::
  ( ExtractFromSpec Required tail result
  ) => ExtractFromSpec Required (RL.Cons label a tail) result

instance iExtractOptionalFromSpecNil ::
  ExtractFromSpec Optional RL.Nil ()

instance iExtractOptionalFromSpecIgnore ::
  ( ExtractFromSpec Optional tail result
  ) => ExtractFromSpec Optional (RL.Cons label (T.Required a) tail) result

else instance iExtractOptionalFromSpecCons ::
  ( ExtractFromSpec Optional tail result'
  , Row.Cons label a result' result
  ) => ExtractFromSpec Optional (RL.Cons label a tail) result

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

