module Main where

import Prelude
import Prim.TypeError
import Type.Row

import Data.Maybe (Maybe(..), optional)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Main4 as Main4
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Equality (class TypeEquals)
import Type.Prelude (class IsSymbol, Proxy(..), RLProxy(..), RProxy(..))
import Type.RowList (class ListToRow) as RL
import Unsafe.Coerce (unsafeCoerce)

data Required a

class Sort
  (i :: RL.RowList) (o :: RL.RowList)
  | i -> o

instance sort ::
  ( RL.ListToRow i i'
  , RL.RowToList i' o
  ) => Sort i o

class ExtractRequiredFromDefinition
  (spec :: RL.RowList) (initial :: RL.RowList) (result :: RL.RowList)
  | spec initial -> result

instance _ExtractRequiredFromDefinitionNil ::
  ( Sort initial initial'
  ) => ExtractRequiredFromDefinition RL.Nil initial initial'

instance _ExtractRequiredFromDefinitionCons ::
  ( ExtractRequiredFromDefinition tail (RL.Cons label a initial) result
  ) => ExtractRequiredFromDefinition (RL.Cons label (Required a) tail) initial result

else instance _ExtractRequiredFromDefinitionIgnore ::
  ( ExtractRequiredFromDefinition tail initial result
  ) => ExtractRequiredFromDefinition (RL.Cons label a tail) initial result

class ExtractOptionalFromDefinition
  (spec :: RL.RowList) (initial :: RL.RowList) (result :: RL.RowList)
  | spec initial -> result

instance _ExtractOptionalFromDefinitionNil ::
  ( Sort initial initial'
  ) => ExtractOptionalFromDefinition RL.Nil initial initial'

instance _ExtractOptionalFromDefinitionIgnore ::
  ( ExtractOptionalFromDefinition tail initial result
  ) => ExtractOptionalFromDefinition (RL.Cons label (Required a) tail) initial result

else instance _ExtractOptionalFromDefinitionCons ::
  ( ExtractOptionalFromDefinition tail (RL.Cons label a initial) result
  ) => ExtractOptionalFromDefinition (RL.Cons label a tail) initial result

class CompareToDefinition
  (required :: RL.RowList) (optional :: RL.RowList) (input :: RL.RowList)

instance _CompareToDefinitionNil ::
  CompareToDefinition RL.Nil RL.Nil RL.Nil

instance _CompareToDefinitionRequired ::
  ( CompareToDefinition tail optional tail'
  ) => CompareToDefinition (RL.Cons label a tail) optional (RL.Cons label a tail')

else instance _CompareToDefinitionOptional ::
  ( CompareToDefinition required tail tail'
  ) => CompareToDefinition required (RL.Cons label a tail) (RL.Cons label a tail')

else instance _CompareToDefinitionIgnoreOptional ::
  ( CompareToDefinition required tail input
  ) => CompareToDefinition required (RL.Cons label a tail) input

class Builder
  (required :: RL.RowList) (optional :: RL.RowList) (input :: RL.RowList) input' from to
  | required optional input input' from -> to
  where
    builder :: RLProxy required -> RLProxy optional -> RLProxy input -> Record input' -> Builder.Builder { | from } { | to }

instance _BuilderRequiredNil ::
  Builder RL.Nil RL.Nil RL.Nil input' from from where
    builder _ _ _ _ = identity

instance _BuilderRequired ::
  ( IsSymbol label
  , Builder tail optional tail' deleted from from'
  , Row.Cons label a deleted input'
  , Row.Cons label a from' to
  , Row.Lacks label from'
  , Row.Lacks label deleted
  ) => Builder (RL.Cons label a tail) optional (RL.Cons label a tail') input' from to where
    builder _ _ _ r = first <<< rest
      where
        label   = SProxy :: _ label
        value   = Record.get label r :: a
        deleted = Record.delete label r
        first   = Builder.insert label value
        rest    = builder (RLProxy :: _ tail) (RLProxy :: _ optional) (RLProxy :: _ tail') deleted

else instance _BuilderOptionalJust ::
  ( IsSymbol label
  , Builder required tail tail' deleted from from'
  , Row.Cons label a deleted input'
  , Row.Cons label (Maybe a) from' to
  , Row.Lacks label from'
  , Row.Lacks label deleted
  ) => Builder required (RL.Cons label a tail) (RL.Cons label a tail') input' from to where
    builder _ _ _ r = first <<< rest
      where
        label   = SProxy :: _ label
        value   = Just $ Record.get label r :: Maybe a
        deleted = Record.delete label r
        first   = Builder.insert label value
        rest    = builder (RLProxy :: _ required) (RLProxy :: _ tail) (RLProxy :: _ tail') deleted

else instance _BuilderOptionalNothing ::
  ( IsSymbol label
  , Builder required tail input input' from from'
  , Row.Cons label (Maybe a) from' to
  , Row.Lacks label from'
  ) => Builder required (RL.Cons label a tail) input input' from to where
    builder _ _ _ r = first <<< rest
      where
        label   = SProxy :: _ label
        value   = Nothing :: Maybe a
        first   = Builder.insert label value
        rest    = builder (RLProxy :: _ required) (RLProxy :: _ tail) (RLProxy :: _ input) r

extract
  :: forall spec spec' required required' optional optional'
   . TypeEquals (RProxy MyRecord) (RProxy spec)
  => RL.RowToList spec spec'
  => ExtractRequiredFromDefinition spec' RL.Nil required'
  => ExtractOptionalFromDefinition spec' RL.Nil optional'
  => RL.ListToRow required' required
  => RL.ListToRow optional' optional
  => TypeEquals { | required } { c :: Int, d :: Number }
  => TypeEquals { | optional } { a :: Int, b :: String, e :: String }
  => {}
extract = {}

extract' :: {}
extract' = extract

confirm
  :: forall spec spec' input input' required' optional'
   . TypeEquals (RProxy MyRecord) (RProxy spec)
  => TypeEquals (RProxy ( a :: Int, c :: Int, d :: Number, e :: String )) (RProxy input)
  => RL.RowToList spec spec'
  => RL.RowToList input input'
  => ExtractRequiredFromDefinition spec' RL.Nil required'
  => ExtractOptionalFromDefinition spec' RL.Nil optional'
  => CompareToDefinition required' optional' input'
  => {}
confirm = {}

confirm' :: {}
confirm' = confirm

class Build
  (spec :: # Type) (input :: # Type) (output :: # Type)
  | spec -> output, output -> spec
  where
    build :: Record input -> Record output

instance i_Build ::
  ( RL.RowToList spec spec'
  , RL.RowToList input input'
  , ExtractRequiredFromDefinition spec' RL.Nil required'
  , ExtractOptionalFromDefinition spec' RL.Nil optional'
  , CompareToDefinition required' optional' input'
  , Builder required' optional' input' input () output
  ) => Build spec input output where
    build r = Builder.build builder' {}
      where
        builder' = builder (RLProxy :: _ required') (RLProxy :: _ optional') (RLProxy :: _ input') r

type MyRecord =
  ( a :: Int
  , b :: String
  , c :: Required Int
  , d :: Required Number
  , e :: String
  )

build1
  :: forall i o
   . Build MyRecord i o
  => Record i
  -> Record o
build1 = build

class Cuild
  (spec :: # Type) (input :: # Type) (output :: # Type) (a :: RL.RowList)
  | spec -> output, output -> spec, spec -> a
  where
    cuild :: Record input -> Record spec -> RLProxy a

instance i_Cuild ::
  ( RL.RowToList spec spec'
  , RL.RowToList input input'
  , ExtractRequiredFromDefinition spec' RL.Nil required'
  , ExtractOptionalFromDefinition spec' RL.Nil optional'
  , CompareToDefinition required' optional' input'
  , Builder required' optional' input' input () output
  ) => Cuild spec input output required' where
    cuild _ _ = RLProxy :: _ required'
-- 
-- a
--   :: forall input output a
--    . Cuild MyRecord input output a
--   => Record input
--   -> Record output
--   -> RLProxy a
-- a = cuild
-- 
-- a' :: _
-- a' = a

a :: _
a r = (r { a: "Hi", b: "aa", c: 3 }).a

a' :: _
a' = a \x -> { a: 3 }

{-
-}

-- -- It still cannot access to field...
-- -- TODO: How can this be inferred?
-- build2
--   :: forall i o
--    . Build MyRecord i o
--   => Record i
--   -> Record o
-- build2 r = result.c
--   where
--     result = build r

-- main :: Effect Unit
-- main = do
--   logShow $ build1

main = Main4.main
