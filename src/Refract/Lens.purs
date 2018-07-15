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

class RecordToLens st (r :: # Type) (rs :: # Type) | r st -> rs, rs st -> r

instance recordToLensInst :: (ListToRow rl r, ListToRow rsl rs, RowToList r rl, RowToList rs rsl, ListToLens st rl rsl) => RecordToLens st r rs

foreign import lensget :: ∀ r rs st s a. RecordToLens st r rs => (ALens' s a -> s -> a) -> Record r -> st -> Record rs

foreign import lensset :: ∀ r rs st s a. RecordToLens st r rs => (ALens' s a -> a -> s -> s) -> Record r -> st -> Record rs -> st

-- Monomorphic lens getter
alensview :: ∀ st a. ALens' st a -> st -> a
alensview l = view (cloneLens l)

-- Monomorphic lens setter
alensset :: ∀ st a. ALens' st a -> a -> st -> st
alensset l = set (cloneLens l)

recordToLens :: ∀ r rs st. RecordToLens st r rs => Record r -> Lens' st (Record rs)
recordToLens r = lens (lensget alensview r) (lensset alensset r)
