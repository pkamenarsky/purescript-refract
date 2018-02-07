module ToDoMVC where
  
--------------------------------------------------------------------------------

import Refract

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Function (on)
import Data.Lens (ALens', Lens', cloneLens, lens, set)
import Data.List (filter, length)
import Data.Ordering (invert)
import Data.Map (Map)
import Data.Map as M
import Data.String as S
import Data.Tuple (fst, snd)
import Prelude (class Ord, Unit, compare, id, not, show, when, ($), (+), (<>), (==), (>))
import Props (_type, autoFocus, checked, className, onBlur, onChange, onClick, onDoubleClick, onEnter, onKeyDown, placeholder, unsafeEventConvert, value)
import React (Event)
import Refract.DOM (div, input, label, span, text)
  
--------------------------------------------------------------------------------

initialState :: AppState
initialState =
  { todos: M.empty
  , todo: ""
  , nextId: 0
  , filter: All
  }

-- | Lenses (`prop @"xyz"` will make this easier in the future).

_todoId :: ∀ r. Lens' { todoId :: Int | r } Int
_todoId = lens (_.todoId) (\x v -> x { todoId = v })

_description :: ∀ r. Lens' { description :: String | r } String
_description = lens (_.description) (\x v -> x { description = v })

_completed :: ∀ r. Lens' { completed :: Boolean | r } Boolean
_completed = lens (_.completed) (\x v -> x { completed = v })

_edited :: ∀ r. Lens' { edited :: Boolean | r } Boolean
_edited = lens (_.edited) (\x v -> x { edited = v })

_input :: ∀ r. Lens' { input :: String | r } String
_input = lens (_.input) (\x v -> x { input = v })

_todos :: ∀ r. Lens' { todos :: Map Int ToDo | r } (Map Int ToDo)
_todos = lens (_.todos) (\x v -> x { todos = v })

_todo :: ∀ r. Lens' { todo :: String | r } String
_todo = lens (_.todo) (\x v -> x { todo = v })

_todo' :: ∀ r. Lens' { todo :: ToDo | r } ToDo
_todo' = lens (_.todo) (\x v -> x { todo = v })

_filter :: ∀ r. Lens' { filter :: ToDoFilter | r } ToDoFilter
_filter = lens (_.filter) (\x v -> x { filter = v })

_temp :: ∀ r. Lens' { temp :: String | r } String
_temp = lens (_.temp) (\x v -> x { temp = v })

_parent :: ∀ st r. Lens' { parent :: st | r } st
_parent = lens (_.parent) (\x v -> x { parent = v })

data ToDoFilter = All | Active | Completed

newtype ToDoIndex = ToDoIndex Int

type ToDo =
  { description :: String
  , completed   :: Boolean

  , edited      :: Boolean
  , input       :: String
  }

type AppState =
  { todos      :: Map Int ToDo
  , todo       :: String
  , nextId     :: Int
  , filter     :: ToDoFilter
  }

targetValue :: Event -> String
targetValue e = (unsafeEventConvert e).target.value

filterMap :: ∀ k v. Ord k => (v -> Boolean) -> Map k v -> Map k v
filterMap f = M.fromFoldable ○ filter (f ○ snd) ○ M.toUnfoldable

data InputResult = Cancel | Input String | Delete

-- | Reusable input component (not specific to ToDoMVC)
-- | * Reacts on enter and escape key and blur events
-- | * Writes the entered text on enter
-- | * Discards the entered text on escape or blur
-- | * Deletes input component on enter, when the text is empty
blurableInput
  :: ∀ eff st.
     ALens' st String                    -- | Lens specifying the target value
  -> (InputResult -> Effect eff st Unit) -- | Result effect operating on the parent state
  -> Component eff st
blurableInput editL result =
  stateL editL \edit -> input
    [ className "todo-edit"
    , autoFocus true
    , value edit
    , onChange \e -> modifyL editL \_ -> targetValue e
    , onKeyDown \e -> if e.keyCode == 13
        then if S.length edit > 0
          then result $ Input edit
          else result Delete
        else when (e.keyCode == 27) (result Cancel)
    , onBlur \_ -> result Cancel
    ] []

checkbox :: ∀ eff st. ALens' st Boolean -> Component eff st
checkbox lns = zoom lns $ state \st -> input
  [ _type "checkbox"
  , className "todo-checkbox"
  , checked st
  , onChange \_ -> modify not
  ] []

inputOnEnter :: ∀ eff st. ALens' st String -> (String -> Effect eff st Unit) -> Component eff st
inputOnEnter lnsStr done = stateL lnsStr \str -> input
  [ className "todo-input"
  , placeholder "What needs to be done?"
  , autoFocus true
  , value str
  , onChange \e -> modifyL lnsStr \_ -> targetValue e
  , onEnter $ when (S.length str > 0) (done str)
  ] []

todoInput :: ∀ eff st.
     (st -> st)
  -> { temp :: ALens' st String
     , current :: ALens' st String
     , active :: ALens' st Boolean
     }
  -> Component eff st
todoInput delete lns = stateR lns \st -> if st.active
  then blurableInput lns.temp \result -> do
    case result of
      Cancel -> modifyR lns \st -> st { temp = "", active = false }
      Input str -> modifyR lns \st -> st { temp = "", active = false, current = st.temp }
      Delete -> modify delete

  else label
    [ className "todo-description"
    , onDoubleClick \_ -> modifyR lns \st -> st { temp = st.current, active = true }
    ]
    [ text st.current ]

spanButton :: ∀ eff st a. ALens' st a -> (a -> a) -> Array (Component eff st) -> Component eff st
spanButton lns f children = span [ onClick \_ -> modifyL lns f ] children

todo
  :: ∀ eff st.
     ALens' st ToDo        -- | Lens to the current todo
  -> (st -> st)            -- | Removes the current item from the list
  -> Component eff st      -- | Todo Component
todo lns' delete = div
  [ className "todo" ]
  [ checkbox (lns ○ _completed)
  , todoInput delete
      { temp: lns ○ _input
      , current: lns ○ _description
      , active: lns ○ _edited
      }
  , div [ className "todo-delete", onClick \_ -> modify delete ] []
  ]
  where
    lns = cloneLens lns'

todoMVC :: ∀ eff st. ALens' st AppState -> Component eff st
todoMVC lns = zoom lns $ state \st -> div [ className "container" ]
  -- Input field
  [ inputOnEnter _todo \str -> modify \st -> st
      { todo = ""
      , nextId = st.nextId + 1
      , todos = M.insert st.nextId
          { description: str
          , completed: false
          , edited: false
          , input: ""
          } st.todos
      }

  -- Individual todos
  , foreachMapF ((invert ○ _) ○ (compare `on` fst)) (visible st.filter ○ snd) _todos todo

  -- Footer
  , div
      [ className "footer" ]
      [ span
          [ className "todo-count"]
          [ text $ show (length $ filter (not _.completed) $ M.values st.todos) <> " items left" ]

      , div
          [ className "todo-filters" ]
          [ spanButton id (set _filter All) [ text "All" ], text "/"
          , spanButton id (set _filter Active) [ text "Active" ], text "/"
          , spanButton id (set _filter Completed) [ text "Completed" ]
          ]

      , if (length $ filter (_.completed) $ M.values st.todos) > 0
          then span
            [ className "todo-clear"
            , onClick \_ -> modify \st -> st { todos = filterMap (not (_.completed)) st.todos }
            ]
            [ text "Clear completed"]
          else span [] []
      ]
  ]
  where
    visible :: ToDoFilter -> ToDo -> Boolean
    visible All _ = true
    visible Active todo = not todo.completed
    visible Completed todo = todo.completed

-- Main ------------------------------------------------------------------------

main :: ∀ eff. AppState -> (AppState -> Eff eff Unit) -> Eff (dom :: DOM | eff) Unit
main = run "main" (todoMVC id)
