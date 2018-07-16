module Refract.Lens
  ( class RecordToLens
  , class ListToLens
  , recordToLens
  ) where

import Data.Lens (ALens', Lens', Shop, cloneLens, lens, set, view)
import Type.Row (Cons, Nil, kind RowList, class RowToList, class ListToRow)

-- If `l` is `Nil`, `st` will be left to be whatever it wants to be.
-- Otherwise `l` will determine what `st` should be, i.e. `l -> r st` would
-- be possible.
class ListToLens st (l :: RowList) (r :: RowList) | l st -> r, r st -> l

instance listToLensNil :: ListToLens st Nil Nil
-- | This is just an expansion of the `ALens' o st` type synonym.
-- In particular, the JS implementation relies on the concrete Shop type,
-- because if there were just a `Strong p` constraint on any type `p`, that
-- constraint would not be passed along to the JS.
else instance listToLensCons :: (ListToLens st ls rs) => ListToLens st (Cons k (Shop o o o o -> Shop o o st st) ls) (Cons k o rs)

class RecordToLens st (r :: Type) (rs :: Type) | r st -> rs, rs st -> r where
  recordToLens :: r -> Lens' st rs

instance recordToLensInst :: (ListToRow rl r, ListToRow rsl rs, RowToList r rl, RowToList rs rsl, ListToLens st rl rsl) => RecordToLens st (Record r) (Record rs) where
  recordToLens r = lens (lensget alensview r) (lensset alensset r)
else instance lensToLensInst :: RecordToLens st (Shop o o o o -> Shop o o st st) o where
  recordToLens r = cloneLens r

foreign import lensget :: ∀ r rs st s a. RecordToLens st r rs => (ALens' s a -> s -> a) -> r -> st -> rs

foreign import lensset :: ∀ r rs st s a. RecordToLens st r rs => (ALens' s a -> a -> s -> s) -> r -> st -> rs -> st

-- Monomorphic lens getter
alensview :: ∀ st a. ALens' st a -> st -> a
alensview l = view (cloneLens l)

-- Monomorphic lens setter
alensset :: ∀ st a. ALens' st a -> a -> st -> st
alensset l = set (cloneLens l)
