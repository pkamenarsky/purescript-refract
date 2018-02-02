module Counters where
  
--------------------------------------------------------------------------------

import Refract (Component, _id, foreachZ, modify, state, run, zoom)
import Refract.DOM (div, text)

import Control.Monad.Eff (Eff)
import Data.Array (cons)
import Data.Tuple (Tuple)
import Data.Lens (_1, _2)
import DOM (DOM)
import Props (onClick)
import Prelude hiding (div)
  
--------------------------------------------------------------------------------

counter :: ∀ eff. Component eff Int
counter = state \st -> div []
  [ div [ onClick \_ -> modify (_ - 1) ] [ text "Decrement" ]
  , text (show st)
  , div [ onClick \_ -> modify (_ + 1) ] [ text "Increment" ]
  ]

twoCounters :: ∀ eff. Component eff (Tuple Int Int)
twoCounters = div [] [ zoom _1 counter, zoom _2 counter ]

manyCounters :: ∀ eff. Component eff (Array Int)
manyCounters = div []
  [ div [ onClick \_ -> modify (cons 0) ] [ text "Add counter" ]
  , foreachZ _id counter
  ]

main :: ∀ eff. Array Int -> (Array Int -> Eff eff Unit) -> Eff (dom :: DOM | eff) Unit
main = run "main" manyCounters
