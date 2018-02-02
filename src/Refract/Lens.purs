module Refract.Lens
  ( class RecordToLens
  , class ListToLens
  , recordToLens
  ) where

import Data.Lens (ALens', Lens', cloneLens, lens, set, view)
import Data.Profunctor.Strong (class Strong)
import Type.Row (Cons, Nil, kind RowList, class RowToList, class ListToRow)

class ListToLens st (l :: RowList) (r :: RowList) | l st -> r, r st -> l

instance listToLensNil :: ListToLens st Nil Nil

instance listToLensCons :: (Strong p, ListToLens st ls rs) => ListToLens st (Cons k (p o o -> p st st) ls) (Cons k o rs)

class RecordToLens st (r :: # Type) (rs :: # Type)

instance recordToLensInst :: (ListToRow rl r, ListToRow rsl rs, RowToList r rl, RowToList rs rsl, ListToLens st rl rsl) => RecordToLens st r rs

foreign import lensget :: ∀ r rs st s a. RecordToLens st r rs => (ALens' s a -> s -> a) -> Record r -> st -> Record rs

foreign import lensset :: ∀ r rs st s a. RecordToLens st r rs => (ALens' s a -> a -> s -> s) -> Record r -> st -> Record rs -> st

alensview :: ∀ st a. ALens' st a -> st -> a
alensview l = view (cloneLens l)

alensset :: ∀ st a. ALens' st a -> a -> st -> st
alensset l = set (cloneLens l)

recordToLens :: ∀ r rs st. RecordToLens st r rs => Record r -> Lens' st (Record rs)
recordToLens r = lens (lensget alensview r) (lensset alensset r)

