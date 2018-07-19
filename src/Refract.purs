module Refract
  ( Component
  , FocusedComponent(FocusedComponent)
  , ComponentClass
  , DeleteAction(DeleteAction)
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
  , ignore
  , liftEffect
  , mapEffect
  , mkComponent
  , modify
  , modify'
  , run
  , state
  , stateCached
  , stateCached2
  , stateCached3
  , showAny
  , trace
  , unfiltered
  , keySort
  , zoom
  , lensAtM'
  , zoomFor
  ) where

import Prelude

import Control.Monad.Free (Free, hoistFree, liftF, runFreeM)
import Data.Array (catMaybes, filter, index, length, range, sortBy, updateAt, zip)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Functor.Invariant (class Invariant)
import Data.Lens (ALens', Lens', Setter', Shop, cloneLens, lens, over, set, (.~), (^.))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect) as E
import Effect.Aff (Aff, makeAff, launchAff_, nonCanceler)
import Effect.Class (liftEffect) as E
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactElement)
import React as R
import React.DOM (IsDynamic(..), mkDOM)
import React.DOM as RD
import React.DOM.Props as P
import ReactDOM as RD
import Refract.Lens (class RecordToLens, recordToLens)
import Undefined (undefined)
import Web.DOM as WD
import Web.DOM.Document as WD
import Web.DOM.NonElementParentNode as WD
import Web.HTML as W
import Web.HTML.HTMLDocument as W
import Web.HTML.Window as W
import Unsafe.Coerce (unsafeCoerce)

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

interpretEffect :: ∀ s a. R.ReactThis {} (Record s) -> Effect (Record s) a -> Aff a
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

type Props s r = (Effect s (Maybe r) -> Aff Unit) -> P.Props

-- Components ------------------------------------------------------------------

type Derivation i s t =
  { state  :: s
  , derive :: s -> t
  , update :: i -> t -> s
  }

newtype FocusedComponent s t r
  = FocusedComponent ((Effect s (Maybe r) -> Aff Unit) -> Lens' s t -> t -> ReactElement)

type Component s = forall t. FocusedComponent t s Unit

-- Zoom, state -----------------------------------------------------------------

foreign import genId :: Unit -> String
foreign import refEq :: ∀ a. a -> a -> Boolean
foreign import logAny :: ∀ a. a -> E.Effect Unit
foreign import showAny :: ∀ a. a -> String
foreign import trace :: ∀ a. String -> a -> a

ignore :: ∀ s t r. FocusedComponent s t Unit -> FocusedComponent s t r
ignore (FocusedComponent cmp) = FocusedComponent \effect l st -> cmp (\eff -> effect $ eff *> pure Nothing) l st

-- | Reify the current `Component` state.
state :: ∀ s t r. (t -> (forall q. Effect t q -> Effect s q) -> FocusedComponent s t r) -> FocusedComponent s t r
state f = FocusedComponent \effect l st -> runComponent (f st (mapEffect l)) effect l st
  where
    runComponent (FocusedComponent cmp) = cmp

-- | Reify the current `Component` state.
stateCached :: ∀ s t r. ((forall q. Effect t q -> Effect s q) -> t -> FocusedComponent s t r) -> FocusedComponent s t r
stateCached f = FocusedComponent \effect lens st ->
  let render st' = runComponent (f (mapEffect lens) st') effect lens st'
  in R.unsafeCreateLeafElement component' { state: st, render }
  where
    runComponent (FocusedComponent cmp) = cmp
    component' = component $ genId unit

stateCached2 :: ∀ a s t r. ((forall q. Effect t q -> Effect s q) -> t -> a -> FocusedComponent s t r) -> a -> FocusedComponent s t r
stateCached2 f = \a -> FocusedComponent \effect lens st ->
  let render st' = runComponent (f (mapEffect lens) st' a) effect lens st'
  in R.unsafeCreateLeafElement component' { state: st, render }
  where
    runComponent (FocusedComponent cmp) = cmp
    component' = component $ genId unit

stateCached3 :: ∀ a b s t r. ((forall q. Effect t q -> Effect s q) -> t -> a -> b -> FocusedComponent s t r) -> a -> b -> FocusedComponent s t r
stateCached3 f = \a b -> FocusedComponent \effect lens st ->
  let render st' = runComponent (f (mapEffect lens) st' a b) effect lens st'
  in R.unsafeCreateLeafElement component' { state: st, render }
  where
    runComponent (FocusedComponent cmp) = cmp
    component' = component $ genId unit

stateCached4 :: ∀ a b c s t r. ((forall q. Effect t q -> Effect s q) -> t -> a -> b -> c -> FocusedComponent s t r) -> a -> b -> c -> FocusedComponent s t r
stateCached4 f = \a b c -> FocusedComponent \effect lens st ->
  let render st' = runComponent (f (mapEffect lens) st' a b c) effect lens st'
  in R.unsafeCreateLeafElement component' { state: st, render }
  where
    runComponent (FocusedComponent cmp) = cmp
    component' = component $ genId unit

zoom :: ∀ ps s t l r q. RecordToLens s l t => l -> FocusedComponent ps t r -> (Maybe r -> Effect ps (Maybe q)) -> FocusedComponent ps s q
zoom l (FocusedComponent cmp) r = FocusedComponent \effect l'' st -> cmp (\eff -> effect $ eff >>= r) (l'' ○ l') (st ^. l')
  where
    l' = recordToLens l

data DeleteAction = DeleteAction

zoomFor :: ∀ ps s t i r q
  .  (s -> Array i)
  -> (i -> ALens' s (Maybe t))
  -> (i -> FocusedComponent ps t DeleteAction)
  -> FocusedComponent ps s Unit
zoomFor keys keylens cmp = FocusedComponent \effect l st ->
  RD.div [] $ flip map (keys st) \key -> case cloneLens (keylens key) of
    keylens' -> case st ^. keylens' of
      Nothing  -> RD.div [] []
      Just st' -> runComponent
        (cmp key)
        (\eff -> effect $ delete (over l (\ps -> keylens' .~ Nothing $ ps)) eff)
        (l ○ lens (const st') (\s t -> keylens' .~ Just t $ s))
        st'

  where
    delete :: (ps -> ps) -> Effect ps (Maybe DeleteAction) -> Effect ps (Maybe Unit)
    delete f action = do
      r <- action
      case r of
        Just DeleteAction -> do
          modify f
          pure $ Just unit
        Nothing -> pure $ Just unit

    runComponent (FocusedComponent cmp) = cmp

-- React -----------------------------------------------------------------------

-- | A `Component st` is parameterized over a state type `st` over which it operates.

-- | A React component class. Useful whenever a `Component` needs to implement
-- | React lifecycle methods.
type ComponentClass s t = ReactClass { effect :: Effect { | s } Unit -> E.Effect Unit, state :: { | s }, lens :: Lens' { | s } t }

-- | React lifecycle spec.
type Spec s t =
  { componentDidMount :: Effect s Unit
  , componentWillUnmount :: Effect s Unit
  , componentDidUpdate :: s -> Effect s Unit

  , shouldComponentUpdate :: s -> Boolean

  , unsafeComponentWillUpdate :: s -> Effect s Unit
  , unsafeComponentWillMount :: Effect s Unit
  }

-- | No-op lifecycle method spec.
defaultSpec :: ∀ s t. Spec s t
defaultSpec =
  { componentDidMount: pure unit
  , componentWillUnmount: pure unit
  , componentDidUpdate: \_ -> pure unit

  , shouldComponentUpdate: \_ -> true

  , unsafeComponentWillUpdate: \_ -> pure unit
  , unsafeComponentWillMount: pure unit
  }

-- | Create a `ComponentClass` from a `Spec` and a `Component`.
-- componentClass
--   :: ∀ s t. Spec { | s } t
--   -> String
--   -> FocusedComponent { | s } t
--   -> ComponentClass { | s } t
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

-- -- | Create a `Component` from a `ComponentClass`.
-- createElement :: ∀ st. ComponentClass st -> Component (Record st)
-- createElement class_ effect state' = R.createLeafElement class_ { effect, state: state' }

-- | Create a DOM element `Component`.
mkComponent
  :: ∀ s t r. String                -- | Element name
  -> Array (Props s r)              -- | Props
  -> Array (FocusedComponent s t r) -- | Children
  -> FocusedComponent s t r
mkComponent element props children = FocusedComponent \effect l st -> mkDOM
  (IsDynamic false) element (map (_ $ effect) props)
  (map (\(FocusedComponent cmp) -> cmp effect l st) children)

-- Run -------------------------------------------------------------------------

-- | Attach `Component` to the DOM element with the specified id and run it.
run
  :: ∀ s. String                           -- | Element id
  -> FocusedComponent { | s } { | s } Unit -- | Component
  -> { | s }
  -> ({ | s } -> E.Effect Unit)            -- | State callback (useful for hot reloading)
  -> E.Effect Unit
run elemId cmp st updateState = void $ element >>= RD.render ui
  where
    ui = R.unsafeCreateLeafElement (R.component "root" $ reactClass cmp) {}

    reactClass (FocusedComponent cmp) this = pure
      { render: do
           st' <- R.getState this
           updateState st'
           pure $ cmp (\effect -> interpretEffect this effect *> pure unit) identity st'
      , state: st
      }

    -- spec :: R.ReactSpec Unit st (Reacteff)
    -- spec = R.spec st \this -> do
    --   st' <- R.getState this
    --   unsafeCoerce$ updateState $ st'
    --   pure $ st' # cmp \effect ->
    --     unsafeCoerce$ launchAff_ $
    --       interpretEffect this (unsafeCoerceEffect effect)

    element :: E.Effect WD.Element
    element = do
      win <- W.window
      doc <- W.document win
      e   <- WD.getElementById elemId (WD.toNonElementParentNode $ W.toDocument doc)
      pure $ fromMaybe undefined e

-- Traversals ------------------------------------------------------------------

lensAtA :: ∀ a. Int -> Lens' (Array a) a
lensAtA i = lens (\m -> unsafePartial $ fromJust $ index m i) (\m v -> fromMaybe m $ updateAt i v m)

lensAtM :: ∀ k v. Ord k => k -> Lens' (Map k v) v
lensAtM k = lens (\m -> unsafePartial $ fromJust $ M.lookup k m) (\m v -> M.insert k v m)

lensAtM' :: ∀ k v. Ord k => k -> ALens' (Map k v) (Maybe v)
lensAtM' k = undefined

data DeleteElement = DeleteElement

foreachA
  :: ∀ s t a
  .  Array a
  -> (Int -> Lens' t a)
  -> (Int -> FocusedComponent s a Unit)
  -> FocusedComponent s t Unit
foreachA cmp = undefined

foreachD
  :: ∀ s t k v
  .  (k -> Lens' t (Maybe v))
  -> (k -> FocusedComponent s v DeleteElement)
  -> FocusedComponent s t Unit
foreachD cmp = undefined

-- | Filtered `Array` traversal.
foreach
  :: ∀ s a
  .  (Int -> FocusedComponent s a Unit)
  -> FocusedComponent s (Array a) Unit
foreach cmp = FocusedComponent \effect l st ->
  let mkElement (i × a) = runComponent (cmp i) effect (cloneLens l ○ lensAtA i) a
      runComponent (FocusedComponent cmp') = cmp'

  in  RD.div [] $ map mkElement $ zip (range 0 (length st - 1)) st

unfiltered :: ∀ a. a -> Boolean
unfiltered _ = true

-- deleteBy :: ∀ k v. (k -> k -> Ordering) -> k -> Map k v -> Map k v
-- deleteBy f k m = coerce M.delete { compare: f } k m
--   where
--     coerce :: (forall k v. Ord k => k -> Map k v -> Map k v) -> { compare :: k -> k -> Ordering } -> k -> Map k v -> Map k v
--     coerce = unsafeCoerce
-- 
-- insertBy :: ∀ k v. (k -> k -> Ordering) -> k -> v -> Map k v -> Map k v
-- insertBy f k m = coerce M.insert { compare: f } k m
--   where
--     coerce :: (forall k v. Ord k => k -> v -> Map k v -> Map k v) -> { compare :: k -> k -> Ordering } -> k -> v -> Map k v -> Map k v
--     coerce = unsafeCoerce
-- 
-- lookupBy :: ∀ k v. (k -> k -> Ordering) -> k -> Map k v -> Maybe v
-- lookupBy f k m = coerce M.lookup { compare: f } k m
--   where
--     coerce :: (forall k v. Ord k => k -> Map k v -> Maybe v) -> { compare :: k -> k -> Ordering } -> k -> Map k v -> Maybe v
--     coerce = unsafeCoerce
-- 
-- lensAtMBy :: ∀ k v. (k -> k -> Ordering) -> k -> Lens' (Map k v) v
-- lensAtMBy ord_f k = lens (\m -> unsafePartial $ fromJust $ lookupBy ord_f k m) (\m v -> insertBy ord_f k v m)
-- 

-- 
-- | Filtered and sorted `Map` traversal.
foreachMap
  :: ∀ s t k v
  .  (k -> String)
  -> (k -> k -> Ordering)
  -> (k × v -> k × v -> Ordering)
  -> (k × v -> Boolean)
  -> (Effect s Unit -> k -> FocusedComponent s v Unit)
  -> FocusedComponent s (Map k v) Unit
foreachMap = undefined -- \show_f keyord_f ord_f filter_f cmp -> FocusedComponent \effect l' st ->
--   let l = cloneLens l'
--       elems = sortBy ord_f ○ filter filter_f
--       mkElement (k × v) = R.fragmentWithKey (show_f k) [ runComponent (cmp (modify $ over l $ deleteBy keyord_f k ) k) effect (l ○ lensAtMBy keyord_f k) v ]
--       runComponent (FocusedComponent cmp') = cmp'
--       render st = RD.div [] $ map mkElement (elems $ M.toUnfoldable st) 
-- 
--   -- in  RD.div [] $ map mkElement (elems $ M.toUnfoldable st) 
--   in R.unsafeCreateLeafElement component' { state: st, render: render }
--   where
--     component' = component $ genId unit

type ThisProps s r = 
  { state :: s
  , render :: s -> R.ReactElement
  | r
  }

type ClassSpec s r =
  { render :: E.Effect ReactElement
  , componentDidMount :: E.Effect Unit
  , shouldComponentUpdate :: { render :: s -> R.ReactElement, state :: s | r } -> {} -> E.Effect Boolean
  }

component :: String -> ∀ s r. R.ReactClass (ThisProps s r)
component className = R.component className reactClass

reactClass
  :: ∀ s r. R.ReactThis (ThisProps s r) {}
  -> E.Effect (ClassSpec s r)
reactClass this = pure
  { render: do
      props <- R.getProps this
      logAny "RENDER"
      pure $ props.render props.state
  , componentDidMount: do
      logAny "MOUNT"
      pure unit
  , shouldComponentUpdate: \oldProps _ -> do
      newProps <- R.getProps this
      pure $ not $ oldProps.state `refEq` newProps.state
  }

keySort :: ∀ k v. Ord k => k × v -> k × v -> Ordering
keySort (k1 × _) (k2 × _) = compare k1 k2
