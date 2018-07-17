module Refract
  ( Component
  , FocusedComponent(FocusedComponent)
  , ComponentClass
  , EffectEF
  , EffectF
  , Effect
  , Props
  , Spec
  , (○)
  , type (×)
  , (×)
  -- , componentClass
  -- , createElement
  , defaultSpec
  , effectfully
  , foreach
  , foreachMap
  , liftEffect
  , mapEffect
  , mkComponent
  , modify
  , modify'
  , state
  , unfiltered
  , keySort
  , zoom
  ) where

import Prelude

import Control.Monad.Free (Free, hoistFree, liftF, runFreeM)
import Data.Array (catMaybes, filter, index, length, sortBy, updateAt)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Functor.Invariant (class Invariant)
import Data.Lens (ALens', Lens', Setter', cloneLens, lens, over, set, (^.))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect) as E
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect) as E
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactElement)
import React as R
import React.DOM (IsDynamic(..), mkDOM)
import React.DOM as RD
import React.DOM.Props as P
import Refract.Lens (class RecordToLens, recordToLens)
import Undefined (undefined)

-- | A synonym for `<<<`.
infixr 9 compose as ○

-- | A synonym for `Tuple`.
infixr 6 Tuple as ×

-- | A type synonym for `Tuple`.
infixr 6 type Tuple as ×

-- Effects ---------------------------------------------------------------------

-- | The base functor of the `Effect` free monad.
newtype EffectF s next = EffectF (Exists (EffectEF s next))

-- | A functor for the existential.
data EffectEF s next r
  = Modify (s -> s × r) (r -> next)
  | Effect (Aff r) (r -> next)

instance invariantEffectEF :: Invariant (EffectEF s next) where
  imap f g = case _ of
    Modify m n -> Modify (map f ○ m) (n ○ g)
    Effect e n -> Effect (map f e) (n ○ g)

instance functorEffectF :: Functor (EffectF s) where
  map f = overEffectF \eef -> mkExists case eef of
    Modify m n -> Modify m (f ○ n)
    Effect e n -> Effect e (f ○ n)

unEffectF :: ∀ s next. EffectF s next -> Exists (EffectEF s next)
unEffectF (EffectF ef) = ef

overEffectF
  :: ∀ s t next next'
   . (∀ a. EffectEF s next a -> Exists (EffectEF t next'))
  -> EffectF s next
  -> EffectF t next'
overEffectF f = EffectF ○ runExists f ○ unEffectF

-- | An `Effect` with base type `st`.
type Effect s = Free (EffectF s)

mapEffectEF :: ∀ s t next a. Lens' t s -> EffectEF s next a -> EffectEF t next a
mapEffectEF lns (Modify f next) = Modify (\st -> let st' × a = f (st ^. lns) in set lns st' st × a) next
mapEffectEF lns (Effect f next) = Effect f next

mapEffectF :: ∀ s t next. Lens' t s -> EffectF s next -> EffectF t next
mapEffectF lns = overEffectF (mkExists ○ mapEffectEF lns)

mapEffect :: ∀ s t a. Lens' t s -> Effect s a -> Effect t a
mapEffect lns m = hoistFree (mapEffectF lns) m

interpretEffect :: ∀ s a. R.ReactThis Unit (Record s) -> Effect (Record s) a -> Aff a
interpretEffect this m = runFreeM (runExists go ○ unEffectF) m
  where
    -- Since we don't know what the particular type of `b` is (it is hidden away
    -- in the existential), we need to make sure that `go` works for any `b`.
    -- Which means that we take the second component of `Modify`/`Effect` and
    -- apply it to the result of the first.
    go :: ∀ next b. EffectEF (Record s) next b -> Aff next
    go (Modify f next) = do
      st <- E.liftEffect $ R.getState this
      let st' × a = f st
      _ <- makeAff \cb -> do
        void $ R.writeStateWithCallback this st' $ cb $ Right st'
        pure nonCanceler
      pure (next a)
    go (Effect f next) = f >>= pure ○ next

-- | Get the current `Component` state.
get :: ∀ s. Effect s s
get = modify' \st -> st × st

-- | Modify the current `Component` state.
modify :: ∀ s. (s -> s) -> Effect s Unit
modify f = modify' \st -> f st × unit

-- | Modify the current `Component` state and return a result.
modify' :: ∀ s a. (s -> s × a) -> Effect s a
modify' f = liftF $ EffectF $ mkExists $ Modify f identity

-- | Perform a `Control.Monad.Aff` action and return a result.
effectfully :: ∀ a s. Aff a -> Effect s a
-- Use `id` here to get the hidden existential to match up with the result type
effectfully f = liftF $ EffectF $ mkExists $ Effect f identity

-- | A synonym for `effectfully`.
liftAff :: ∀ a s. Aff a -> Effect s a
liftAff = effectfully

-- | Perform a `Monad.Effect` action and return a result.
liftEffect :: ∀ a s. E.Effect a -> Effect s a
liftEffect f = liftF $ EffectF $ mkExists $ Effect (E.liftEffect f) identity

-- Props -----------------------------------------------------------------------

type Props s = (Effect s Unit -> E.Effect Unit) -> P.Props

-- Components ------------------------------------------------------------------

newtype FocusedComponent s t
  = FocusedComponent ((Effect s Unit -> E.Effect Unit) -> Lens' s t -> t -> ReactElement)

type Component s = forall t. FocusedComponent t s

-- Zoom, state -----------------------------------------------------------------

-- | Reify the current `Component` state.
state :: ∀ s t. (t -> (Effect t Unit -> Effect s Unit) -> FocusedComponent s t) -> FocusedComponent s t
state f = FocusedComponent \effect l st -> runComponent (f st (mapEffect l)) effect l st
  where
    runComponent (FocusedComponent cmp) = cmp

zoom :: ∀ ps s t l. RecordToLens s l t => l -> FocusedComponent ps t -> FocusedComponent ps s
zoom l (FocusedComponent cmp) = FocusedComponent \effect l'' st -> cmp (effect $ _) (l'' ○ l') (st ^. l')
  where
    l' = recordToLens l

-- React -----------------------------------------------------------------------

-- | A `Component st` is parameterized over a state type `st` over which it operates.

-- | A React component class. Useful whenever a `Component` needs to implement
-- | React lifecycle methods.
type ComponentClass s = ReactClass { effect :: Effect { | s } Unit -> E.Effect Unit, state :: { | s } }

-- | React lifecycle spec.
type Spec s =
  { componentDidMount :: Effect s Unit
  , componentWillUnmount :: Effect s Unit
  , componentDidUpdate :: s -> Effect s Unit

  , shouldComponentUpdate :: s -> Boolean

  , unsafeComponentWillUpdate :: s -> Effect s Unit
  , unsafeComponentWillMount :: Effect s Unit
  }

-- | No-op lifecycle method spec.
defaultSpec :: ∀ s. Spec s
defaultSpec =
  { componentDidMount: pure unit
  , componentWillUnmount: pure unit
  , componentDidUpdate: \_ -> pure unit

  , shouldComponentUpdate: \_ -> true

  , unsafeComponentWillUpdate: \_ -> pure unit
  , unsafeComponentWillMount: pure unit
  }

-- -- | Create a `ComponentClass` from a `Spec` and a `Component`.
-- componentClass
--   :: ∀ st. Spec { | st }
--   -> String
--   -> Component { | st }
--   -> ComponentClass st
-- componentClass spec displayName cmp = R.component displayName reactClass
--   where
--     reactClass this = pure
--       { render: do
--           props <- R.getProps this
--           pure $ cmp props.effect props.state
--       , state: {}
--       , componentDidMount: do
--           props <- R.getProps this
--           props.effect spec.componentDidMount
--       , shouldComponentUpdate: \props _ -> do
--           pure $ spec.shouldComponentUpdate props.state
--       , componentDidUpdate: \props _ _ -> do
--           props.effect $ spec.componentDidUpdate props.state
--       , componentWillUnmount: do
--           props <- R.getProps this
--           props.effect spec.componentWillUnmount
--       , unsafeComponentWillMount: do
--           props <- R.getProps this
--           props.effect spec.unsafeComponentWillMount
--       , unsafeComponentWillUpdate: \props _ -> do
--           props.effect $ spec.unsafeComponentWillUpdate props.state
--       }
-- 
-- -- | Create a `Component` from a `ComponentClass`.
-- createElement :: ∀ st. ComponentClass st -> Component (Record st)
-- createElement class_ effect state' = R.createLeafElement class_ { effect, state: state' }

-- | Create a DOM element `Component`.
mkComponent
  :: ∀ s t. String                  -- | Element name
  -> Array (Props s)                -- | Props
  -> Array (FocusedComponent s t)   -- | Children
  -> FocusedComponent s t
mkComponent element props children = FocusedComponent \effect l st -> mkDOM
  (IsDynamic false) element (map (_ $ effect) props)
  (map (\(FocusedComponent cmp) -> cmp effect l st) children)

-- Run -------------------------------------------------------------------------

-- | Attach `Component` to the DOM element with the specified id and run it.
-- run
--   :: ∀ st. String     -- | Element id
--   -> Component st     -- | Component
--   -> st                   -- | Initial state
--   -> (st -> Unit) -- | State callback (useful for hot reloading)
--   -> (dom :: DOM | eff) Unit
-- run elemId cmp st updateState = void $ element >>= render ui
--   where
--     ui = R.createFactory (R.createClass spec) unit
-- 
--     spec :: R.ReactSpec Unit st (Reacteff)
--     spec = R.spec st \this -> do
--       st' <- R.getState this
--       unsafeCoerce$ updateState $ st'
--       pure $ st' # cmp \effect ->
--         unsafeCoerce$ launchAff_ $
--           interpretEffect this (unsafeCoerceEffect effect)
-- 
--     element :: (dom :: DOM | eff) Element
--     element = do
--       win <- window
--       doc <- document win
--       e   <- getElementById (ElementId elemId) (documentToNonElementParentNode (htmlDocumentToDocument doc))
--       pure $ fromMaybe undefined e

-- Traversals ------------------------------------------------------------------

lensAtA :: ∀ a. Int -> Lens' (Array a) a
lensAtA i = lens (\m -> unsafePartial $ fromJust $ index m i) (\m v -> fromMaybe m $ updateAt i v m)

lensAtM :: ∀ k v. Ord k => k -> Lens' (Map k v) v
lensAtM k = lens (\m -> unsafePartial $ fromJust $ M.lookup k m) (\m v -> M.insert k v m)

foreign import mapI :: ∀ a. Int -> (Int -> a) -> Array a

-- | Unfiltered `Array` traversal.
-- foreach
--   :: ∀ st a.
--      ALens' st (Array a)
--   -> (ALens' st a -> Component st)
--   -> Component st
-- foreach = undefined -- foreachF (const true)
-- 
-- -- | Filtered `Array` traversal.
-- foreachF
--   :: ∀ st a.
--      (a -> Boolean)
--   -> ALens' st (Array a)
--   -> (ALens' st a -> Component st)
--   -> Component st
-- foreachF f lns' cmp effect st = undefined
--   -- RD.div [] $ catMaybes $ mapI (length st') \i -> case index st' i >>= pure ○ f of
--   --   Just true  -> Just $ cmp (lns ○ lensAtA i) effect st
--   --   Just false -> Nothing
--   --   Nothing    -> Nothing
--   -- where
--   --   lns = cloneLens lns'
--   --   st' = st ^. lns

-- | Zooming unfiltered `Array` traversal.
-- TODO: traversable
foreach
  :: ∀ s a.
     (a -> Boolean)
  -> FocusedComponent s a
  -> FocusedComponent s (Array a)
foreach = undefined -- foreachZF (const true)

unfiltered :: ∀ a. a -> Boolean
unfiltered _ = true

-- | Filtered `Map` traversal.
foreachMap
  :: ∀ s t k v. Ord k
  => (k × v -> k × v -> Ordering)
  -> (k × v -> Boolean)
  -> (Effect s Unit -> FocusedComponent s t)
  -> FocusedComponent s (Map k v)
foreachMap ord_f filter_f cmp = undefined
  -- RD.div [] $ flip map (elems $ M.toUnfoldable (st ^. lns)) \(k × v) -> cmp (lns ○ lensAtM k) (over lns (M.delete k)) effect st
  -- where
  --   lns = cloneLens lns'
  --   elems = sortBy ord_f ○ filter filter_f

keySort :: ∀ k v. Ord k => k × v -> k × v -> Ordering
keySort (k1 × _) (k2 × _) = compare k1 k2
