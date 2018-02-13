module Refract
  ( Component
  , ComponentClass
  , EffectEF
  , EffectF
  , Effect
  , Handler
  , Props
  , Spec
  , Unzoom
  , (○)
  , type (×)
  , (×)
  , componentClass
  , component
  , defaultSpec
  , effectfully
  , foreach
  , foreachF
  , foreachU
  , foreachUF
  , foreachZ
  , foreachZF
  , foreachMap
  , foreachMapF
  , foreachMapU
  , foreachMapUF
  , mkComponent
  , modify
  , modifyL
  , modifyR
  , run
  , state
  , stateL
  , stateR
  , zoom
  , zoomUn
  , zoomR
  , zoomRUn
  ) where

import Prelude

import Control.Monad.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free (Free, hoistFree, liftF, runFreeM)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
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
import React (ReactClass, ReactElement, createClass, createFactory)
import React as R
import React.DOM (IsDynamic(..), mkDOM)
import React.DOM as RD
import React.DOM.Props as P
import ReactDOM (render)
import Refract.Lens (class RecordToLens, recordToLens)
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

-- | A synonym for `<<<`.
infixr 9 compose as ○

-- | A synonym for `Tuple`.
infixr 6 Tuple as ×

-- | A type synonym for `Tuple`.
infixr 6 type Tuple as ×

-- Effects ---------------------------------------------------------------------

-- | The base functor of the `Effect` free monad.
newtype EffectF eff st next = EffectF (Exists (EffectEF eff st next))

-- | A functor for the existential.
data EffectEF eff st next r
  = Modify (st -> st × r) (r -> next)
  | Effect (st -> Aff eff r) (r -> next)

-- | Slightly safer coerce to change effect types on the base functor
unsafeCoerceEffectF :: ∀ eff eff' st next. EffectF eff st next -> EffectF eff' st next
unsafeCoerceEffectF = unsafeCoerce

-- | Slightly safe coerce to change effect types on the free monad
unsafeCoerceEffect :: ∀ eff eff' st a. Effect eff st a -> Effect eff' st a
unsafeCoerceEffect = unsafeCoerce

instance invariantEffectEF :: Invariant (EffectEF eff st next) where
  imap f g = case _ of
    Modify m n -> Modify (map f <<< m) (n <<< g)
    Effect e n -> Effect (map f <<< e) (n <<< g)

instance functorEffectF :: Functor (EffectF eff st) where
  map f = overEffectF \eef -> mkExists case eef of
    Modify m n -> Modify m (f <<< n)
    Effect e n -> Effect e (f <<< n)

unEffectF :: ∀ eff st next. EffectF eff st next -> Exists (EffectEF eff st next)
unEffectF (EffectF ef) = ef

overEffectF
  :: ∀ eff st st' next next'
   . (∀ a. EffectEF eff st next a -> Exists (EffectEF eff st' next'))
  -> EffectF eff st next
  -> EffectF eff st' next'
overEffectF f = EffectF <<< runExists f <<< unEffectF

-- | An `Effect` with base type `st`.
type Effect eff st = Free (EffectF eff st)

type ReactEff eff = (state :: R.ReactState R.ReadWrite, console :: CONSOLE | eff)

mapEffectEF :: ∀ eff st stt next a. Lens' st stt -> EffectEF eff stt next a -> EffectEF eff st next a
mapEffectEF lns (Modify f next) = Modify (\st -> let st' × a = f (st ^. lns) in set lns st' st × a) next
mapEffectEF lns (Effect f next) = Effect (\st -> f (st ^. lns)) next

mapEffectF :: ∀ eff st stt next. Lens' st stt -> EffectF eff stt next -> EffectF eff st next
mapEffectF lns = overEffectF (mkExists <<< mapEffectEF lns)

mapEffect :: ∀ eff st stt a. Lens' st stt -> Effect eff stt a -> Effect eff st a
mapEffect lns m = hoistFree (mapEffectF lns) m

interpretEffect :: ∀ eff st a. R.ReactThis Unit st -> Effect (ReactEff eff) st a -> Aff (ReactEff eff) a
interpretEffect this m = runFreeM (runExists go <<< unEffectF) m
  where
    -- Since we don't know what the particular type of `b` is (it is hidden away
    -- in the existential), we need to make sure that `go` works for any `b`.
    -- Which means that we take the second component of `Modify`/`Effect` and
    -- apply it to the result of the first.
    go :: ∀ next b. EffectEF (ReactEff eff) st next b -> Aff (ReactEff eff) next
    go (Modify f next) = do
      st <- liftEff $ R.readState this
      let st' × a = f st
      _ <- makeAff \cb -> do
        void $ R.writeStateWithCallback this st' (cb $ Right st')
        pure nonCanceler
      pure (next a)
    go (Effect f next) = do
      st <- liftEff $ R.readState this
      f st >>= pure ○ next

-- | Modify the current `Component` state.
modify :: ∀ eff st. (st -> st) -> Effect eff st Unit
modify f = modify' \st -> f st × unit

-- | Modify the current `Component` state and return a result.
modify' :: ∀ eff st a. (st -> st × a) -> Effect eff st a
modify' f = liftF $ EffectF $ mkExists $ Modify f id

-- | Perform a `Control.Monad.Aff` action and return a result.
effectfully :: ∀ a eff st. (st -> Aff eff a) -> Effect eff st a
-- Use `id` here to get the hidden existential to match up with the result type
effectfully f = liftF $ EffectF $ mkExists $ Effect f id

-- Zoom, state, effects --------------------------------------------------------

-- | Reify the current `Component` state.
state :: ∀ eff st. (st -> Component eff st) -> Component eff st
state f effect st = (f st) effect st

-- | Reify `Component` substate specified by a lens.
stateL
  :: ∀ eff st stt. ALens' st stt
  -> (stt -> Component eff st)
  -> Component eff st
stateL lns f effect st = (f (st ^. cloneLens lns)) effect st

-- | `stateL` for generic records.
-- |
-- | Specializing to a concrete state, its type would be:
-- |
-- | ```purescript
-- | stateR
-- |   :: ∀ eff st.
-- |      { name :: ALens' st String
-- |      , age :: ALens' st Int
-- |      }
-- |   -> ( { name :: String
-- |        , age :: Int
-- |        } -> Component eff st
-- |      )
-- |   -> Component eff st
-- | ```
stateR
  :: ∀ eff r rs st. RecordToLens st r rs
  => Record r
  -> (Record rs -> Component eff st)
  -> Component eff st
stateR r f effect st = (f (st ^. recordToLens r)) effect st

-- | Modify `Component` substate specified by a `Lens`.
modifyL
  :: ∀ eff st stt. ALens' st stt
  -> (stt -> stt)
  -> Effect eff st Unit
modifyL lns f = modify (over (cloneLens lns) f)

-- | Modify `Component` substate with any kind of `Setter`.
modifyS
  :: ∀ eff st stt. Setter' st stt
  -> (stt -> stt)
  -> Effect eff st Unit
modifyS lns f = modify (over lns f)

-- | Modify `Component` substate specified by a `Lens` and return a result.
modifyL'
  :: ∀ eff st stt a. ALens' st stt
  -> (stt -> stt × a)
  -> Effect eff st a
modifyL' lns' f = modify' (\st -> let st' × a = f (st ^. lns) in set lns st' st × a)
  where
    lns = cloneLens lns'

-- | `modifyL` for generic records.
-- |
-- | Specializing to a concrete state, its type would be:
-- |
-- | ```purescript
-- | modifyR
-- |   :: ∀ eff st.
-- |      { name :: ALens' st String
-- |      , age :: ALens' st Int
-- |      }
-- |   -> ({ name :: String , age :: Int } -> { name :: String , age :: Int })
-- |   -> Effect eff st Unit
-- | ```
modifyR
  :: ∀ eff r rs st. RecordToLens st r rs
  => Record r
  -> (Record rs -> Record rs)
  -> Effect eff st Unit
modifyR r f = modify (over (recordToLens r) f)

-- | Like `modifyR` but return a result.
modifyR'
  :: ∀ eff r rs st a. RecordToLens st r rs
  => Record r
  -> (Record rs -> Record rs × a)
  -> Effect eff st a
modifyR' r f = modify' (\st -> let st' × a = f (st ^. lns) in set lns st' st × a)
  where
    lns = recordToLens r

type Unzoom eff st stt = Component eff st -> Component eff stt

-- | Embed a `Component` with a state of type `stt` into a `Component`
-- | with a state of type `st`.
zoom
  :: ∀ eff st stt. ALens' st stt
  -> Component eff stt
  -> Component eff st
zoom lns cmp effect st = cmp (\e -> effect $ mapEffect (cloneLens lns) e) (st ^. cloneLens lns)

-- | Embed a `Component` with a state of type `stt` into a `Component`
-- | with a state of type `st` and provide it with an unzoom combinator.
zoomUn
  :: ∀ eff st stt. ALens' st stt
  -> (Unzoom eff st stt -> Component eff stt)
  -> Component eff st
zoomUn lns cmp effect st = cmp (\cmp' _ _ -> cmp' effect st) (\e -> effect $ mapEffect lns' e) (st ^. lns')
  where lns' = cloneLens lns

-- | `zoom` for generic records.
-- |
-- | Specializing to a concrete state, its type would be:
-- |
-- | ``` purescript
-- | zoomR
-- |   :: ∀ eff st.
-- |      { name :: ALens' st String
-- |      , age :: ALens' st Int
-- |      }
-- |   -> Component eff
-- |        { name :: String
-- |        , age :: Int
-- |        }
-- |   -> Component eff st
-- | ```
zoomR
  :: ∀ eff r rs st. RecordToLens st r rs
  => Record r
  -> Component eff (Record rs)
  -> Component eff st
zoomR r cmp effect st = cmp (\e -> effect $ mapEffect (recordToLens r) e) (st ^. recordToLens r)

-- | `zoomUn` for generic records.
zoomRUn
  :: ∀ eff r rs st. RecordToLens st r rs
  => Record r
  -> (Unzoom eff st (Record rs)
  -> Component eff (Record rs))
  -> Component eff st
zoomRUn r cmp effect st = cmp (\cmp' _ _ -> cmp' effect st) (\e -> effect $ mapEffect (recordToLens r) e) (st ^. recordToLens r)

-- Props -----------------------------------------------------------------------

type Handler st = R.EventHandlerContext R.ReadWrite Unit st Unit

type Props eff st = (Effect eff st Unit -> Handler st) -> P.Props

-- Components ------------------------------------------------------------------


-- | A `Component eff st` is parameterized over an effect type `eff` and a
-- | state type `st` over which it operates.
type Component eff st = (Effect eff st Unit -> Handler st) -> st -> ReactElement

-- | A React component class. Useful whenever a `Component` needs to implement
-- | React lifecycle methods.
type ComponentClass eff st = ReactClass ((Effect eff st Unit -> Handler st) × st)

-- | React lifecycle spec.
type Spec eff st =
  { displayName :: String

  , componentWillMount :: Effect eff st Unit
  , componentDidMount :: Effect eff st Unit
  , componentWillUnmount :: Effect eff st Unit

  , componentWillUpdate :: st -> Effect eff st Unit
  , componentDidUpdate :: st -> Effect eff st Unit

  , shouldComponentUpdate :: st -> Boolean
  }

-- | No-op lifecycle method spec.
defaultSpec :: ∀ eff st. Spec eff st
defaultSpec =
  { displayName: ""
  , componentWillMount: pure unit
  , componentDidMount: pure unit
  , componentWillUnmount: pure unit

  , componentWillUpdate: \_ -> pure unit
  , componentDidUpdate: \_ -> pure unit

  , shouldComponentUpdate: \_ -> true
  }

-- | Create a `ComponentClass` from a `Spec` and a `Component`.
componentClass
  :: ∀ eff st. Spec eff st
  -> Component eff st
  -> ComponentClass eff st
componentClass spec cmp = createClass
  { render: \this -> do
      (effect × st) <- R.getProps this
      pure $ cmp effect st
  , displayName: spec.displayName
  , getInitialState: \_ -> pure unit
  , componentWillMount: \this -> do
      (effect × _) <- R.getProps this
      unsafeCoerce $ effect spec.componentWillMount
  , componentDidMount: \this -> do
      (effect × _) <- R.getProps this
      effect spec.componentDidMount
  , componentWillReceiveProps: \_ _ -> pure unit
  , shouldComponentUpdate: \_ (_ × newst) _ -> pure $ spec.shouldComponentUpdate newst
  , componentWillUpdate: \this (_ × newst) _ -> do
      (effect × _) <- R.getProps this
      effect $ spec.componentWillUpdate newst
  , componentDidUpdate: \this (_ × oldst) _ -> do
      (effect × _) <- R.getProps this
      unsafeCoerce $ effect $ spec.componentDidUpdate oldst
  , componentWillUnmount: \this -> do
      (effect × _) <- R.getProps this
      unsafeCoerce $ effect spec.componentWillUnmount
  }

-- | Create a `Component` from a `ComponentClass`.
component :: ∀ eff st. ComponentClass eff st -> Component eff st
component class_ effect st = createFactory class_ (effect × st)

-- | Create a DOM element `Component`.
mkComponent
  :: ∀ eff st. String         -- | Element name
  -> Array (Props eff st)     -- | Props
  -> Array (Component eff st) -- | Children
  -> Component eff st
mkComponent element props children effect st = mkDOM (IsDynamic false) element (map (\p -> p effect) props) (map (\e -> e effect st) children)

-- Run -------------------------------------------------------------------------

-- | Attach `Component` to the DOM element with the specified id and run it.
run
  :: ∀ eff st. String     -- | Element id
  -> Component eff st     -- | Component
  -> st                   -- | Initial state
  -> (st -> Eff eff Unit) -- | State callback (useful for hot reloading)
  -> Eff (dom :: DOM | eff) Unit
run elemId cmp st updateState = void $ element >>= render ui
  where
    ui = R.createFactory (R.createClass spec) unit

    spec :: R.ReactSpec Unit st (ReactEff eff)
    spec = R.spec st \this -> do
      st' <- R.readState this
      unsafeCoerceEff $ updateState $ st'
      pure $ st' # cmp \effect ->
        unsafeCoerceEff $ launchAff_ $
          interpretEffect this (unsafeCoerceEffect effect)

    element :: Eff (dom :: DOM | eff) Element
    element = do
      win <- window
      doc <- document win
      e   <- getElementById (ElementId elemId) (documentToNonElementParentNode (htmlDocumentToDocument doc))
      pure $ fromMaybe undefined e

--------------------------------------------------------------------------------

lensAtA :: ∀ a. Int -> Lens' (Array a) a
lensAtA i = lens (\m -> unsafePartial $ fromJust $ index m i) (\m v -> fromMaybe m $ updateAt i v m)

lensAtM :: ∀ k v. Ord k => k -> Lens' (Map k v) v
lensAtM k = lens (\m -> unsafePartial $ fromJust $ M.lookup k m) (\m v -> M.insert k v m)

foreign import mapI :: ∀ a. Int -> (Int -> a) -> Array a

-- | Unfiltered `Array` traversal.
foreach
  :: ∀ eff st a.
     ALens' st (Array a)
  -> (ALens' st a -> Component eff st)
  -> Component eff st
foreach = foreachF (const true)

-- | Filtered `Array` traversal.
foreachF
  :: ∀ eff st a.
     (a -> Boolean)
  -> ALens' st (Array a)
  -> (ALens' st a -> Component eff st)
  -> Component eff st
foreachF f lns' cmp effect st =
  RD.div [] $ catMaybes $ mapI (length st') \i -> case index st' i >>= pure ○ f of
    Just true  -> Just $ cmp (lns ○ lensAtA i) effect st
    Just false -> Nothing
    Nothing    -> Nothing
  where
    lns = cloneLens lns'
    st' = st ^. lns

-- | Unfiltered `Array` traversal providing an unzoom combinator.
foreachU
  :: ∀ eff st a.
     ALens' st (Array a)
  -> (Unzoom eff st a -> Component eff a)
  -> Component eff st
foreachU = foreachUF (const true)

-- | Filtered `Array` traversal providing an unzoom combinator.
foreachUF
  :: ∀ eff st a.
     (a -> Boolean)
  -> ALens' st (Array a)
  -> (Unzoom eff st a -> Component eff a)
  -> Component eff st
foreachUF f lns cmp = foreachF f lns \lns' -> zoomUn lns' \unzoom -> cmp unzoom

-- | Zooming unfiltered `Array` traversal.
foreachZ
  :: ∀ eff st a.
     ALens' st (Array a)
  -> Component eff a
  -> Component eff st
foreachZ = foreachZF (const true)

-- | Zooming filtered `Array` traversal.
foreachZF :: ∀ eff st a.
     (a -> Boolean)
  -> ALens' st (Array a)
  -> Component eff a
  -> Component eff st
foreachZF f lns cmp = foreachF f lns \lns' -> zoom lns' cmp

-- | Unfiltered `Map` traversal.
foreachMap
  :: ∀ eff st k v. Ord k
  => (k × v -> k × v -> Ordering)
  -> ALens' st (Map k v)
  -> (ALens' st v -> (st -> st) -> Component eff st)
  -> Component eff st
foreachMap ord_f = foreachMapF ord_f (const true)

-- | Filtered `Map` traversal.
foreachMapF
  :: ∀ eff st k v. Ord k
  => (k × v -> k × v -> Ordering)
  -> (k × v -> Boolean)
  -> ALens' st (Map k v)
  -> (ALens' st v -> (st -> st) -> Component eff st)
  -> Component eff st
foreachMapF ord_f filter_f lns' cmp effect st =
  RD.div [] $ flip map (elems $ M.toUnfoldable (st ^. lns)) \(k × v) -> cmp (lns ○ lensAtM k) (over lns (M.delete k)) effect st
  where
    lns = cloneLens lns'
    elems = sortBy ord_f ○ filter filter_f

-- | Unfiltered `Map` traversal providing an unzoom combinator.
foreachMapU
  :: ∀ eff st k v. Ord k
  => (k × v -> k × v -> Ordering)
  -> ALens' st (Map k v)
  -> (Unzoom eff st v -> (st -> st) -> Component eff v)
  -> Component eff st
foreachMapU ord_f = foreachMapUF ord_f (const true)

-- | Filtered `Map` traversal providing an unzoom combinator.
foreachMapUF
  :: ∀ eff st k v. Ord k
  => (k × v -> k × v -> Ordering)
  -> (k × v -> Boolean)
  -> ALens' st (Map k v)
  -> (Unzoom eff st v -> (st -> st) -> Component eff v)
  -> Component eff st
foreachMapUF ord_f filter_f lns cmp =
  foreachMapF ord_f filter_f lns \lns' delete ->
    zoomUn lns' \unzoom -> cmp unzoom delete
