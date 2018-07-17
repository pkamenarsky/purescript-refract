module Counters where
  
--------------------------------------------------------------------------------

import Refract (Component, FocusedComponent, foreach, modify, run, state, unfiltered, zoom, zoomL)
import Refract.DOM (div, text)

import Data.Array (cons)
import Effect (Effect)
import Data.Symbol (SProxy(SProxy))
import Data.Tuple (Tuple(Tuple))
import Data.Lens (Lens', _1, _2)
import Data.Lens.Record (prop)
import Props (onClick)
import Prelude hiding (div)
import Undefined (undefined)
  
--------------------------------------------------------------------------------

type AppState =
  { c :: Int
  , d :: Int
  }

_c :: ∀ r. Lens' { c :: Int | r } Int
_c = prop (SProxy :: SProxy "c")

_d :: ∀ r. Lens' { d :: Int | r } Int
_d = prop (SProxy :: SProxy "d")

counter :: Component Int
counter = state \st embed -> div []
  [ div [ onClick \_ -> embed $ modify (_ - 1) ] [ text "Decrement" ]
  , text (show st)
  , div [ onClick \_ -> embed $ modify (_ + 1) ] [ text "Increment" ]
  ]

counterA :: Component AppState
counterA = div []
  [ zoom _c counter
  , zoom _d counter
  ]

twoCounters :: Component (Tuple Int Int)
twoCounters = div []
  [ zoom _1 counter
  , zoom _2 counter
  ]

manyCounters :: Component (Array Int)
manyCounters = state \_ embed -> div []
  [ div [ onClick \_ -> embed $ modify (cons 0) ] [ text "Add counter" ]
  , foreach unfiltered counter
  ]

-- Main ------------------------------------------------------------------------

-- main :: Array Int -> (Array Int -> Effect Unit) -> Effect Unit
-- main = run "main" (manyCounters identity)

main :: Effect Unit
main = run "main" counterA { c: 0, d: 0 } (\_ -> pure unit)
