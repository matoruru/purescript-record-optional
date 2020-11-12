module Build where

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Prelude (identity, (<<<))
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Type as T
import Type.Prelude (class IsSymbol, class TypeEquals, RLProxy(..), RProxy)

class Build
  (spec :: # Type) (input :: # Type) (result :: Type)
  | spec -> result
  where
    build :: T.Spec spec -> Record input -> result

instance iBuild ::
  ( RL.RowToList spec spec'
  , RL.RowToList input input'
  , FromSpec spec' result
  , Builder spec' input' input result
  ) => Build spec input (Record result) where
  build _ = Builder.build builder'
    where
      builder' :: Builder.Builder (Record input) (Record result)
      builder' = builder (RLProxy :: _ spec') (RLProxy :: _ input')

class Builder
  (spec :: RL.RowList) (input :: RL.RowList) (from :: # Type) (to :: # Type)
  | spec input from -> to
  where
    builder :: RLProxy spec -> RLProxy input -> Builder.Builder { | from } { | to }

instance iBuilderNil :: Builder RL.Nil RL.Nil from from where
    builder _ _ = identity

instance iBuilderRequired ::
  ( Row.Cons label a () expected
  , Row.Cons label' a' () actual
  , TypeEquals (RProxy expected) (RProxy actual)
  , Builder tail tail' from to
  ) => Builder (RL.Cons label (T.Required a) tail) (RL.Cons label' a' tail') from to where
    builder _ _ = builder (RLProxy :: _ tail) (RLProxy :: _ tail')

else instance iBuilderOptionalFound ::
  ( TypeEquals a' (Maybe a'')
  , TypeEquals a a''
  , Builder tail tail' from to
  ) => Builder (RL.Cons label a tail) (RL.Cons label a' tail') from to where
    builder _ _ = builder (RLProxy :: _ tail) (RLProxy :: _ tail')

else instance iBuilderOptionalMissing ::
  ( IsSymbol label
  , Row.Lacks label from
  , Row.Cons label (Maybe a) from from'
  , Builder tail input from' to
  ) => Builder (RL.Cons label a tail) input from to where
    builder _ _ = next <<< Builder.insert label value
      where
        label = SProxy :: _ label
        value = Nothing :: Maybe a
        next  = builder (RLProxy :: _ tail) (RLProxy :: _ input)

class FromSpec (spec :: RL.RowList) (result :: # Type) | spec -> result

instance iFromSpec :: FromSpecI spec () result => FromSpec spec result

class FromSpecI (spec :: RL.RowList) (row :: # Type) (result :: # Type) | spec row -> result

instance iFromSpecINil :: FromSpecI RL.Nil row row

instance iFromSpecIRequired ::
  ( Row.Cons label a row result
  , FromSpecI tail result result'
  ) => FromSpecI (RL.Cons label (T.Required a) tail) row result'

else instance iFromSpecIOptional ::
  ( Row.Cons label (Maybe a) row result
  , FromSpecI tail result result'
  ) => FromSpecI (RL.Cons label a tail) row result'

