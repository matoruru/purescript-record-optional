module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..), reflectSymbol)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, class TypeEquals, Proxy(..), RLProxy(..), RProxy(..))
import Unsafe.Coerce (unsafeCoerce)

insert :: forall k v. IsSymbol k => SProxy k -> v -> Object v -> Object v
insert = Object.insert <<< reflectSymbol

lookup :: forall k v. IsSymbol k => SProxy k -> Object v -> Maybe v
lookup = Object.lookup <<< reflectSymbol

main :: Effect Unit
main = do
  let
    a = Object.empty
    b = insert (SProxy :: _ "a") 3 a
    c = (insert) (SProxy :: _ "b") "Hello" b
    d = insert (SProxy :: _ "c") 5 c

  logShow $ lookup (SProxy :: _ "b") d
  logShow d

  pure unit

--data Required a
--
--class MapRecord (spec :: RL.RowList) (input :: # Type) (output :: Type)
--  | spec input -> output where
--  mapRecordBuilder :: RLProxy spec -> Record input -> Map String a
--
--instance mapRecordCons ::
--  ( IsSymbol name
--  , MapRecord tail from from'
--  , Row.Lacks name from'
--  , Row.Cons name (Maybe a) from' to
--  ) => MapRecord (RL.Cons name a tail) from to where
--  mapRecordBuilder _ =
--    first <<< rest
--    where
--      nameP = SProxy :: _ name
--      rest = mapRecordBuilder (RLProxy :: _ tail)
--      first = Builder.insert nameP (Nothing :: _ a)
--
--instance mapRecordNil :: MapRecord RL.Nil () () where
--  mapRecordBuilder _ = identity
--
--type MyType =
--  ( a :: Int
--  , b :: String
--  , c :: Number
--  , d :: String
--  , e :: Required Int
--  , f :: Required String
--  )
