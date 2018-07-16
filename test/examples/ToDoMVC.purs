module ToDoMVC where
  
--------------------------------------------------------------------------------

import Refract

-- import DOM (DOM)
import Data.Function (on)
import Data.Int (round)
import Data.Lens (ALens', Lens', cloneLens, set)
import Data.Lens.Record (prop)
import Data.List (filter, length)
import Data.Ordering (invert)
import Data.Map (Map)
import Data.Map as M
import Data.String as S
import Data.Symbol (SProxy(SProxy))
import Data.Tuple (fst, snd)
import Prelude (class Ord, Unit, bind, compare, identity, not, pure, show, when, unit, ($), (+), (<>), (==), (>))
import Props (_type, autoFocus, checked, className, onBlur, onChange, onClick, onDoubleClick, onEnter, onKeyDown, placeholder, value)
import React.SyntheticEvent as Event
import Refract.DOM (div, input, label, span, text)
import Unsafe.Coerce (unsafeCoerce)
import Undefined (undefined)
  
--------------------------------------------------------------------------------

initialState :: AppState
initialState =
  { todos: M.empty
  , todo: ""
  , nextId: 0
  , filter: All
  }

-- | Lenses

type S = SProxy

s :: ∀ a. SProxy a
s = SProxy

_todoId :: ∀ r. Lens' { todoId :: Int | r } Int
_todoId = prop (s :: S "todoId")

_description :: ∀ r. Lens' { description :: String | r } String
_description = prop (s :: S "description")

_completed :: ∀ r. Lens' { completed :: Boolean | r } Boolean
_completed = prop (s :: S "completed")

_edited :: ∀ r. Lens' { edited :: Boolean | r } Boolean
_edited = prop (s :: S "edited")

_input :: ∀ r. Lens' { input :: String | r } String
_input = prop (s :: S "input")

_todos :: ∀ r. Lens' { todos :: Map Int ToDo | r } (Map Int ToDo)
_todos = prop (s :: S "todos")

_todo :: ∀ r. Lens' { todo :: String | r } String
_todo = prop (s :: S "todo")

_todo' :: ∀ r. Lens' { todo :: ToDo | r } ToDo
_todo' = prop (s :: S "todo")

_filter :: ∀ r. Lens' { filter :: ToDoFilter | r } ToDoFilter
_filter = prop (s :: S "filter")

_temp :: ∀ r. Lens' { temp :: String | r } String
_temp = prop (s :: S "temp")

_parent :: ∀ st r. Lens' { parent :: st | r } st
_parent = prop (s :: S "parent")

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

filterMap :: ∀ k v. Ord k => (v -> Boolean) -> Map k v -> Map k v
filterMap f = M.fromFoldable ○ filter (f ○ snd) ○ M.toUnfoldable

data InputResult = Cancel | Input String | Delete

-- | Reusable input component (not specific to ToDoMVC)
-- | * Reacts on enter and escape key and blur events
-- | * Writes the entered text on enter
-- | * Discards the entered text on escape or blur
-- | * Deletes input component on enter, when the text is empty
blurableInput
  :: ∀ st s.
     (InputResult -> Effect st String Unit) -- | Result effect operating on the parent state
  -> FocusedComponent st String
blurableInput result = state \st unzoom -> input
    [ className "todo-edit"
    , autoFocus true
    , value st
    , onChange \e -> do
        target <- liftEffect $ Event.target e
        modify \_ -> (unsafeCoerce target).value
    , onKeyDown \e -> do
        keyCode <- liftEffect $ Event.keyCode e
        if round keyCode == 13
          then if S.length st > 0
            then result $ Input st
            else result Delete
          else when (round keyCode == 27) (result Cancel)
    , onBlur \_ -> result Cancel
    ] []

checkbox :: Component Boolean
checkbox = state \st _ -> input
  [ _type "checkbox"
  , className "todo-checkbox"
  , checked st
  , onChange \_ -> modify not
  ] []

inputOnEnter :: ∀ st. (String -> Effect st String Unit) -> FocusedComponent st String
inputOnEnter done = state \str _ -> input
  [ className "todo-input"
  , placeholder "What needs to be done?"
  , autoFocus true
  , value str
  , onChange \e -> do
      target <- liftEffect $ Event.target e
      modify \_ -> (unsafeCoerce target).value
  , onEnter $ when (S.length str > 0) (done str)
  ] []

type T = 
       { temp :: String
       , current :: String
       , active :: Boolean
       }


todoInput :: ∀ st.
     (Effect st T Unit)
  -> FocusedComponent st
       { temp :: String
       , current :: String
       , active :: Boolean
       }
todoInput delete = state \st _ -> if st.active
  then zoom _temp $ blurableInput \result -> do
    case result of
      Cancel -> undefined -- modify \st' -> st' { temp = "", active = false }
      Input str -> undefined -- modify \st' -> st' { temp = "", active = false, current = st.temp }
      Delete -> undefined
  else div [] []
  -- then blurableInput lns.temp \result -> do
  --   case result of
  --     Cancel -> modifyR lns \st' -> st' { temp = "", active = false }
  --     Input str -> modifyR lns \st' -> st' { temp = "", active = false, current = st.temp }
  --     Delete -> modify delete

  -- else label
  --   [ className "todo-description"
  --   , onDoubleClick \_ -> modifyR lns \st' -> st' { temp = st.current, active = true }
  --   ]
  --   [ text st.current ]

-- spanButton :: ∀ st a. ALens' st a -> (a -> a) -> Array (Component st) -> Component st
-- spanButton lns f children = span [ onClick \_ -> modifyL lns f ] children
-- 
-- todo
--   :: ∀ st.
--      ALens' st ToDo  -- | Lens to the current todo
--   -> (st -> st)      -- | Removes the current item from the list
--   -> Component st    -- | Todo Component
-- todo lns' delete = div
--   [ className "todo" ]
--   [ checkbox (lns ○ _completed)
--   , todoInput delete
--       { temp: lns ○ _input
--       , current: lns ○ _description
--       , active: lns ○ _edited
--       }
--   , div [ className "todo-delete", onClick \_ -> modify delete ] []
--   ]
--   where
--     lns = cloneLens lns'
-- 
-- todoMVC :: ∀ st. ALens' st AppState -> Component st
-- todoMVC lns = zoom lns $ state \st -> div [ className "container" ]
--   -- Input field
--   [ inputOnEnter _todo \str -> modify \st' -> st
--       { todo = ""
--       , nextId = st'.nextId + 1
--       , todos = M.insert st'.nextId
--           { description: str
--           , completed: false
--           , edited: false
--           , input: ""
--           } st.todos
--       }
-- 
--   -- Individual todos
--   , foreachMapF ((invert ○ _) ○ (compare `on` fst)) (visible st.filter ○ snd) _todos todo
-- 
--   -- Footer
--   , div
--       [ className "footer" ]
--       [ span
--           [ className "todo-count"]
--           [ text $ show (length $ filter (not _.completed) $ M.values st.todos) <> " items left" ]
-- 
--       , div
--           [ className "todo-filters" ]
--           [ spanButton identity (set _filter All) [ text "All" ], text "/"
--           , spanButton identity (set _filter Active) [ text "Active" ], text "/"
--           , spanButton identity (set _filter Completed) [ text "Completed" ]
--           ]
-- 
--       , if (length $ filter (_.completed) $ M.values st.todos) > 0
--           then span
--             [ className "todo-clear"
--             , onClick \_ -> modify \st' -> st' { todos = filterMap (not (_.completed)) st.todos }
--             ]
--             [ text "Clear completed"]
--           else span [] []
--       ]
--   ]
--   where
--     visible :: ToDoFilter -> ToDo -> Boolean
--     visible All _ = true
--     visible Active todo' = not todo'.completed
--     visible Completed todo' = todo'.completed
-- 
-- -- Main ------------------------------------------------------------------------
-- 
-- -- main :: ∀ eff. AppState -> (AppState -> Eff eff Unit) -> Eff (dom :: DOM | eff) Unit
-- -- main = run "main" (todoMVC id)
