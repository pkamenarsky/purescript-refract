module Counters where
  
--------------------------------------------------------------------------------

import Prelude hiding (div)

import Data.Array (cons)
import Data.Function (on)
import Data.Int (round)
import Data.Lens (ALens', Lens', cloneLens, set)
import Data.Lens (Lens', _1, _2)
import Data.Lens.Record (prop)
import Data.Lens.Record (prop)
import Data.List (filter, length)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(Nothing))
import Data.Ordering (invert)
import Data.String as S
import Data.Symbol (SProxy(SProxy))
import Data.Symbol (SProxy(SProxy))
import Data.Tuple (Tuple(Tuple))
import Data.Tuple (fst, snd)
import Effect as E
import Effect as E
import Prelude (class Ord, Unit, bind, compare, flip, identity, not, pure, show, when, unit, ($), (+), (<>), (==), (>))
import Refract.Props (_type, autoFocus, checked, className, onBlur, onChange, onClick, onDoubleClick, onEnter, onKeyDown, placeholder, value)
import Refract.Props (className, key, value, onChange)
import Refract.Props (onClick)
import React.SyntheticEvent as Event
import Refract (Component, Effect, Component', embed, trace, showAny, modify, run, cache, state, zoom, zoom', liftEffect)
import Refract.DOM (div, input, label, span, text)
import Refract.DOM (div, input, text)
import Undefined (undefined)
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)
  
--------------------------------------------------------------------------------

type AppState =
  { c :: Int
  , d :: Int
  , many :: Map Int Int
  }

_c :: ∀ r. Lens' { c :: Int | r } Int
_c = prop (SProxy :: SProxy "c")

_d :: ∀ r. Lens' { d :: Int | r } Int
_d = prop (SProxy :: SProxy "d")

_many :: ∀ r. Lens' { many :: Map Int Int | r } (Map Int Int)
_many = prop (SProxy :: SProxy "many")

counter :: Component { key :: String } Int
counter = cache $ state \_ st -> div []
  [ div [ onClick \_ -> modify (_ - 1) ] [ text "Decrement" ]
  , text (show st)
  , div [ onClick \_ -> modify (_ + 1) ] [ text "Increment" ]
  ]

twoCounters :: Component {} AppState
twoCounters = div []
  [ zoom' _c counter { key: "counter1" }
  , zoom' _d counter { key: "counter2" }
  ]

manyCounters :: Component {} (Map Int Int)
manyCounters = state \_ st -> div []
  [ div [ onClick \_ -> modify \m -> M.insert (M.size m) 0 m ] [ text "Add counter" ]
  , counters st
  ]
  where
    counters st = div [] $ flip map (M.toUnfoldable st) \(Tuple k v) -> embed
      counter
      { key: show k }
      v
      (modify <<< M.insert k)
      (const $ pure Nothing)

-- Main ------------------------------------------------------------------------

-- main :: Array Int -> (Array Int -> Effect Unit) -> Effect Unit
-- main = run "main" (manyCounters identity)

main :: E.Effect Unit
main = run "main" (zoom' _many manyCounters {}) ({ c: 0, d: 0, many: M.empty } :: AppState) (\_ -> pure unit)
