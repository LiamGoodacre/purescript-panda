module Util.Exists2
  ( Exists2
  , mkExists2
  , runExists2
  , Exists4
  , mkExists4
  , runExists4
  ) where

import Unsafe.Coerce (unsafeCoerce)


foreign import data Exists2 ∷ (Type → Type → Type) → Type


mkExists2 ∷ ∀ f a b. f a b → Exists2 f
mkExists2 = unsafeCoerce


runExists2 ∷ ∀ f r. (∀ x y. f x y → r) → Exists2 f → r
runExists2 = unsafeCoerce


foreign import data Exists4 ∷ (Type -> Type -> Type -> Type -> Type) -> Type

mkExists4 ∷ ∀ f a b c d. f a b c d -> Exists4 f
mkExists4 = unsafeCoerce

runExists4 ∷ ∀ f r. (∀ a b c d. f a b c d -> r) -> Exists4 f -> r
runExists4 = unsafeCoerce

