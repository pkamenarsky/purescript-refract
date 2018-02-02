module Undefined (undefined) where

import Unsafe.Coerce (unsafeCoerce)
import Prelude (unit)

undefined :: forall a. a
undefined = unsafeCoerce unit
