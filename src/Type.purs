module Type where

data Required a

data Spec (spec :: # Type) = Spec

type ApplyFlipped a b = b a

infixl 1 type ApplyFlipped as -*
