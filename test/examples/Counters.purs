module Counters where
  
--------------------------------------------------------------------------------

import Refract (Component, foreachZ, modify, state, zoom)
import Refract.DOM (div, text)

import Data.Array (cons)
import Data.Tuple (Tuple)
import Data.Lens (_1, _2)
import Props (onClick)
import Prelude hiding (div)
  
--------------------------------------------------------------------------------

counter :: Component Int
counter = state \st -> div []
  [ div [ onClick \_ -> modify (_ - 1) ] [ text "Decrement" ]
  , text (show st)
  , div [ onClick \_ -> modify (_ + 1) ] [ text "Increment" ]
  ]

twoCounters :: Component (Tuple Int Int)
twoCounters = div [] [ zoom _1 counter, zoom _2 counter ]

manyCounters :: Component (Array Int)
manyCounters = div []
  [ div [ onClick \_ -> modify (cons 0) ] [ text "Add counter" ]
  , foreachZ identity counter
  ]
