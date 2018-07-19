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
  -- , foreach
  , ignore
  , liftEffect
  , mapEffect
  , mkComponent
  , modify
  , modify'
  , run
  , state
  , showAny
  , trace
  , keySort
  , zoom
  , lensAtM
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

type Props s r = (Effect s (Maybe r) -> E.Effect Unit) -> P.Props

-- Components ------------------------------------------------------------------

-- | A `Component st` is parameterized over a state type `st` over which it operates.
newtype FocusedComponent s r
  = FocusedComponent ((Effect s (Maybe r) -> E.Effect Unit) -> s -> ReactElement)

type Component s = FocusedComponent s Unit

-- Zoom, state -----------------------------------------------------------------

foreign import genId :: Unit -> String
foreign import refEq :: ∀ a. a -> a -> Boolean
foreign import logAny :: ∀ a. a -> E.Effect Unit
foreign import showAny :: ∀ a. a -> String
foreign import trace :: ∀ a. String -> a -> a
foreign import memo :: ∀ a b. (a -> b) -> a -> b
foreign import memo2 :: ∀ a b c. (a -> b -> c) -> a -> b -> c
foreign import memo3 :: ∀ a b c d. (a -> b -> c -> d) -> a -> b -> c -> d

ignore :: ∀ s r. FocusedComponent s Unit -> FocusedComponent s r
ignore (FocusedComponent cmp) = FocusedComponent \effect st -> cmp (\eff -> effect $ eff *> pure Nothing) st

-- | Reify the current `Component` state.
state :: ∀ s r. (s -> FocusedComponent s r) -> FocusedComponent s r
state f = FocusedComponent \effect st -> runComponent (f st) effect st
  where
    runComponent (FocusedComponent cmp) = cmp

cache :: ∀ s r. FocusedComponent s r -> FocusedComponent s r
cache = \cmp -> FocusedComponent \effect st -> 
  let render st' = runComponent cmp effect st'
  in R.unsafeCreateLeafElement component' { state: st, render }
  where
    runComponent (FocusedComponent cmp) = cmp
    component' = component $ genId unit

zoom :: ∀ s t l r q. RecordToLens s l t => l -> FocusedComponent t r -> (Maybe r -> Effect s (Maybe q)) -> FocusedComponent s q
zoom l (FocusedComponent cmp) r = FocusedComponent \effect st -> cmp (\eff -> effect (mapEffect l' eff >>= r)) (st ^. l')
  where
    l' = recordToLens l

data DeleteAction = DeleteAction

zoomFor :: ∀ s t i r q
  .  Show i
  => (s -> Array i)
  -> (i -> ALens' s (Maybe t))
  -> (i -> FocusedComponent t DeleteAction)
  -> FocusedComponent s Unit
zoomFor keys keylens cmp = FocusedComponent \effect st ->
  RD.div [] $ flip map (keys st) \key -> case cloneLens (keylens key) of
    keylens' -> case st ^. keylens' of
      Nothing  -> RD.div [] []
      Just st' -> RD.div [ P.key $ show key ] [ runComponent
        (cmp key)
        undefined -- (\eff -> effect $ delete (\ps -> keylens' .~ Nothing $ ps) eff)
        st'
        ]

  where
    delete :: (s -> s) -> Effect s (Maybe DeleteAction) -> Effect s (Maybe Unit)
    delete f action = do
      r <- action
      case r of
        Just DeleteAction -> do
          modify f
          pure $ Just unit
        Nothing -> pure $ Just unit

    runComponent (FocusedComponent cmp) = cmp

-- React -----------------------------------------------------------------------

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
  :: ∀ s t r. String              -- | Element name
  -> Array (Props s r)            -- | Props
  -> Array (FocusedComponent s r) -- | Children
  -> FocusedComponent s r
mkComponent element props children = FocusedComponent \effect st -> mkDOM
  (IsDynamic false) element (map (_ $ effect) props)
  (map (\(FocusedComponent cmp) -> cmp effect st) children)

-- Run -------------------------------------------------------------------------

-- | Attach `Component` to the DOM element with the specified id and run it.
run
  :: ∀ s. String                   -- | Element id
  -> FocusedComponent { | s } Unit -- | Component
  -> { | s }
  -> ({ | s } -> E.Effect Unit)    -- | State callback (useful for hot reloading)
  -> E.Effect Unit
run elemId cmp st updateState = void $ element >>= RD.render ui
  where
    ui = R.unsafeCreateLeafElement (R.component "root" $ reactClass cmp) {}

    reactClass (FocusedComponent cmp) this = pure
      { render: do
           st' <- R.getState this
           updateState st'
           pure $ cmp (\effect -> launchAff_ $ interpretEffect this effect) st'
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

lensAtM :: ∀ k v. Ord k => k -> ALens' (Map k v) (Maybe v)
lensAtM k = lens (M.lookup k) \m v -> case v of
  Just v' -> M.insert k v' m
  Nothing -> M.delete k m

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
