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
  , mkComponent
  , modify
  , modify'
  , state
  , unfiltered
  , keySort
  , zoom
  ) where

import Prelude

import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect (Effect) as E
import Effect.Class (liftEffect) as E
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
newtype EffectF st s next = EffectF (Exists (EffectEF st s next))

-- | A functor for the existential.
data EffectEF st s next r
  = Modify (s -> s × r) (r -> next)
  | Effect (s -> Aff r) (r -> next)

instance invariantEffectEF :: Invariant (EffectEF st s next) where
  imap f g = case _ of
    Modify m n -> Modify (map f ○ m) (n ○ g)
    Effect e n -> Effect (map f ○ e) (n ○ g)

instance functorEffectF :: Functor (EffectF st s) where
  map f = overEffectF \eef -> mkExists case eef of
    Modify m n -> Modify m (f ○ n)
    Effect e n -> Effect e (f ○ n)

unEffectF :: ∀ st s next. EffectF st s next -> Exists (EffectEF st s next)
unEffectF (EffectF ef) = ef

overEffectF
  :: ∀ st s t next next'
   . (∀ a. EffectEF st s next a -> Exists (EffectEF st t next'))
  -> EffectF st s next
  -> EffectF st t next'
overEffectF f = EffectF ○ runExists f ○ unEffectF

-- | An `Effect` with base type `st`.
type Effect st s = Free (EffectF st s)

mapEffectEF :: ∀ st s t next a. Lens' t s -> EffectEF st s next a -> EffectEF st t next a
mapEffectEF lns (Modify f next) = Modify (\st -> let st' × a = f (st ^. lns) in set lns st' st × a) next
mapEffectEF lns (Effect f next) = Effect (\st -> f (st ^. lns)) next

mapEffectF :: ∀ st s t next. Lens' t s -> EffectF st s next -> EffectF st t next
mapEffectF lns = overEffectF (mkExists ○ mapEffectEF lns)

mapEffect :: ∀ st s t a. Lens' t s -> Effect st s a -> Effect st t a
mapEffect lns m = hoistFree (mapEffectF lns) m

interpretEffect :: ∀ st a. R.ReactThis Unit (Record st) -> Effect (Record st) (Record st) a -> Aff a
interpretEffect this m = runFreeM (runExists go ○ unEffectF) m
  where
    -- Since we don't know what the particular type of `b` is (it is hidden away
    -- in the existential), we need to make sure that `go` works for any `b`.
    -- Which means that we take the second component of `Modify`/`Effect` and
    -- apply it to the result of the first.
    go :: ∀ next b. EffectEF (Record st) (Record st) next b -> Aff next
    go (Modify f next) = do
      st <- E.liftEffect $ R.getState this
      let st' × a = f st
      _ <- makeAff \cb -> do
        void $ R.writeStateWithCallback this st' $ cb $ Right st'
        pure nonCanceler
      pure (next a)
    go (Effect f next) = do
      st <- E.liftEffect $ R.getState this
      f st >>= pure ○ next

-- | Modify the current `Component` state.
modify :: ∀ st s. (s -> s) -> Effect st s Unit
modify f = modify' \st -> f st × unit

-- | Modify the current `Component` state and return a result.
modify' :: ∀ st s a. (s -> s × a) -> Effect st s a
modify' f = liftF $ EffectF $ mkExists $ Modify f identity

-- | Perform a `Control.Monad.Aff` action and return a result.
effectfully :: ∀ a st s. (s -> Aff a) -> Effect st s a
-- Use `id` here to get the hidden existential to match up with the result type
effectfully f = liftF $ EffectF $ mkExists $ Effect f identity

-- | Perform a `Monad.Effect` action and return a result.
liftEffect :: ∀ a st s. E.Effect a -> Effect st s a
liftEffect f = liftF $ EffectF $ mkExists $ Effect (E.liftEffect ○ const f) identity

-- Zoom, state, effects --------------------------------------------------------

-- | Reify the current `Component` state.
state :: ∀ st s. (s -> (Effect st s Unit -> Effect st st Unit) -> FocusedComponent st s) -> FocusedComponent st s
state f = FocusedComponent \effect l st -> runComponent (f st (mapEffect l)) effect l st
  where
    runComponent (FocusedComponent cmp) = cmp

zoom :: ∀ st s t. Lens' s t -> FocusedComponent st t -> FocusedComponent st s
zoom l (FocusedComponent cmp) = FocusedComponent \effect l' st -> cmp (\eff -> effect $ mapEffect l eff) (l' ○ l) (st ^. l)

-- Props -----------------------------------------------------------------------

type Props st s = (Effect st s Unit -> E.Effect Unit) -> P.Props

-- Components ------------------------------------------------------------------

newtype FocusedComponent st s
  = FocusedComponent ((Effect st s Unit -> E.Effect Unit) -> Lens' st s -> s -> ReactElement)

type Component s = forall st. FocusedComponent st s

-- | A `Component st` is parameterized over a state type `st` over which it operates.

-- | A React component class. Useful whenever a `Component` needs to implement
-- | React lifecycle methods.
type ComponentClass st s = ReactClass { effect :: Effect { | st } s Unit -> E.Effect Unit, state :: { | st } }

-- | React lifecycle spec.
type Spec st s =
  { componentDidMount :: Effect st s Unit
  , componentWillUnmount :: Effect st s Unit
  , componentDidUpdate :: st -> Effect st s Unit

  , shouldComponentUpdate :: st -> Boolean

  , unsafeComponentWillUpdate :: st -> Effect st s Unit
  , unsafeComponentWillMount :: Effect st s Unit
  }

-- | No-op lifecycle method spec.
defaultSpec :: ∀ st s. Spec st s
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
  :: ∀ st s. String                  -- | Element name
  -> Array (Props st s)              -- | Props
  -> Array (FocusedComponent st s)   -- | Children
  -> FocusedComponent st s
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

--------------------------------------------------------------------------------

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
  :: ∀ st a.
     (a -> Boolean)
  -> FocusedComponent st a
  -> FocusedComponent st (Array a)
foreach = undefined -- foreachZF (const true)

unfiltered :: ∀ a. a -> Boolean
unfiltered _ = true

-- | Filtered `Map` traversal.
foreachMap
  :: ∀ st s k v. Ord k
  => (k × v -> k × v -> Ordering)
  -> (k × v -> Boolean)
  -> (Effect st s Unit -> FocusedComponent st s)
  -> FocusedComponent st (Map k v)
foreachMap ord_f filter_f cmp = undefined
  -- RD.div [] $ flip map (elems $ M.toUnfoldable (st ^. lns)) \(k × v) -> cmp (lns ○ lensAtM k) (over lns (M.delete k)) effect st
  -- where
  --   lns = cloneLens lns'
  --   elems = sortBy ord_f ○ filter filter_f

keySort :: ∀ k v. Ord k => k × v -> k × v -> Ordering
keySort (k1 × _) (k2 × _) = compare k1 k2
