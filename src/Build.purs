module Build where

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Prelude (Unit, identity, unit, (<<<))
import Prim.Row as Row
import Prim.RowList as RL
import Prim.TypeError (class Fail, Beside, Text)
import Record as Record
import Record.Builder as Builder
import Type as T
import Type.Prelude (class IsSymbol, class TypeEquals, Proxy(..), RLProxy(..), RProxy)

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

instance iBuilderRequiredFound ::
  ( IsSymbol label
  , Row.Cons label a from' from
  , Builder tail tail' from to
  ) => Builder (RL.Cons label (T.Required a) tail) (RL.Cons label a tail') from to where
    builder _ _ = next <<< Builder.modify label identity
      where
        label = SProxy :: _ label
        next  = builder (RLProxy :: _ tail) (RLProxy :: _ tail')

else instance iBuilderRequiredWrongType ::
  ( Row.Cons label a () expected
  , Row.Cons label a' () actual
  , TypeEquals { | expected } { | actual }
  , Builder RL.Nil RL.Nil from to
  ) => Builder (RL.Cons label (T.Required a) tail) (RL.Cons label a' tail') from to where
    builder _ _ = builder (RLProxy :: _ RL.Nil) (RLProxy :: _ RL.Nil)

else instance iBuilderRequiredMissing ::
  ( Fail (Beside (Text "Required field \"") (Beside (Text label) (Beside (Text "\" ") (Text "is missing."))))
  , Builder RL.Nil RL.Nil from to
  ) => Builder (RL.Cons label (T.Required a) tail) (RL.Cons label' a' tail') from to where
    builder _ _ = builder (RLProxy :: _ RL.Nil) (RLProxy :: _ RL.Nil)

else instance iBuilderOptionalFound ::
  ( IsSymbol label
  , Row.Cons label (Maybe a) from' from
  , Builder tail tail' from to
  ) => Builder (RL.Cons label a tail) (RL.Cons label (Maybe a) tail') from to where
    builder _ _ = next <<< Builder.modify label identity
      where
        label = SProxy :: _ label
        next  = builder (RLProxy :: _ tail) (RLProxy :: _ tail')

else instance iBuilderOptionalWrongType ::
  ( Row.Cons label a () expected
  , Row.Cons label (Maybe a') () actual
  , TypeEquals { | expected } { | actual }
  , Builder RL.Nil RL.Nil from to
  ) => Builder (RL.Cons label a tail) (RL.Cons label (Maybe a') tail') from to where
    builder _ _ = builder (RLProxy :: _ RL.Nil) (RLProxy :: _ RL.Nil)

else instance iBuilderOptionalMissingJust ::
  ( IsSymbol label
  , TypeEquals a (Maybe a)
  , Builder RL.Nil RL.Nil from to
  ) => Builder (RL.Cons label a tail) (RL.Cons label a tail') from to where
    builder _ _ = builder (RLProxy :: _ RL.Nil) (RLProxy :: _ RL.Nil)

else instance iBuilderOptionalMissing ::
  ( IsSymbol label
  , Row.Lacks label from
  , Row.Cons label (Maybe a) from from'
  , Builder tail (RL.Cons label' a' tail') from' to
  ) => Builder (RL.Cons label a tail) (RL.Cons label' a' tail') from to where
    builder _ _ = next <<< Builder.insert label value
      where
        label = SProxy :: _ label
        value = Nothing :: Maybe a
        next  = builder (RLProxy :: _ tail) (RLProxy :: _ (RL.Cons label' a' tail'))

else instance iBuilderOptionalMissingNil ::
  ( IsSymbol label
  , Row.Lacks label from
  , Row.Cons label (Maybe a) from from'
  , Builder tail RL.Nil from' to
  ) => Builder (RL.Cons label a tail) RL.Nil from to where
    builder _ _ = next <<< Builder.insert label value
      where
        label = SProxy :: _ label
        value = Nothing :: Maybe a
        next  = builder (RLProxy :: _ tail) (RLProxy :: _ RL.Nil)

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

