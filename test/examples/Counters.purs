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
import Refract (Component, Effect, Component', trace, showAny, modify, run, state, zoom, liftEffect)
import Refract.DOM (div, input, label, span, text)
import Refract.DOM (div, input, text)
import Undefined (undefined)
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)
  
--------------------------------------------------------------------------------

type AppState =
  { c :: Int
  , d :: Int
  , many :: Array Int
  , name :: String
  }

_c :: ∀ r. Lens' { c :: Int | r } Int
_c = prop (SProxy :: SProxy "c")

_d :: ∀ r. Lens' { d :: Int | r } Int
_d = prop (SProxy :: SProxy "d")

_many :: ∀ r. Lens' { many :: Array Int | r } (Array Int)
_many = prop (SProxy :: SProxy "many")

_name :: ∀ r. Lens' { name :: String | r } String
_name = prop (SProxy :: SProxy "name")

counter :: Component {} Int
counter = state \_ st -> div []
  [ div [ onClick \_ -> modify (_ - 1) *> pure Nothing ] [ text "Decrement" ]
  , text (show st)
  , div [ onClick \_ -> modify (_ + 1) *> pure Nothing ] [ text "Increment" ]
  ]

counterA :: Component {} AppState
counterA = div []
  [ zoom _c counter {} (const $ pure Nothing)
  , zoom _d counter {} (const $ pure Nothing)
  ]

-- twoCounters :: Component (Tuple Int Int)
-- twoCounters = div []
--   [ zoom _1 (counter "Counter: ")
--   , zoom _2 (counter "Counter: ")
--   ]

inputOnEnter :: Component {} String
inputOnEnter = state \_ str -> input
  [ className "todo-input"
  , value str
  -- , onChange \e -> do
  --     target <- liftEffect $ Event.target e
  --     embed $ modify \_ -> (unsafeCoerce target).value
  ] []

manyCounters :: Component {} AppState
manyCounters = div []
  [ zoom _name inputOnEnter {} (const $ pure Nothing)
  -- , zoom _many) (const $ pure Nothing) $ state \st -> div []
  --     [ div [ onClick \_ -> modify (cons 0) *> pure Nothing ] [ text "Add counter" ]
  --     -- , foreach unfiltered indexedCounter
  --     ]
  ]

-- Main ------------------------------------------------------------------------

-- main :: Array Int -> (Array Int -> Effect Unit) -> Effect Unit
-- main = run "main" (manyCounters identity)

main :: E.Effect Unit
main = run "main" manyCounters { c: 0, d: 0, many: [], name: "" } (\_ -> pure unit)
