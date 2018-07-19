module ToDoMVC where
  
--------------------------------------------------------------------------------

import Refract

import Data.Function (on)
import Data.Int (round)
import Data.Lens (ALens', Lens', cloneLens, set)
import Data.Lens.Record (prop)
import Data.List (filter, length)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord (compare)
import Data.Ordering (invert)
import Data.String as S
import Data.Symbol (SProxy(SProxy))
import Data.Tuple (fst, snd)
import Effect as E
import Prelude (class Ord, Unit, bind, const, compare, discard, flip, identity, map, not, pure, show, when, unit, ($), (+), (<>), (==), (>))
import React.SyntheticEvent as Event
import Refract.DOM (div, input, label, span, text)
import Refract.Props (_type, autoFocus, checked, className, onBlur, onChange, onClick, onDoubleClick, onEnter, onKeyDown, placeholder, value)
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)
  
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

data InputResult = Cancel | Input String | Delete

-- | Reusable input component (not specific to ToDoMVC)
-- | * Reacts on enter and escape key and blur events
-- | * Writes the entered text on enter
-- | * Discards the entered text on escape or blur
-- | * Deletes input component on enter, when the text is empty
blurableInput
  :: ∀ s. FocusedComponent s String InputResult
blurableInput = stateCached \embed st -> input
    [ className "todo-edit"
    , autoFocus true
    , value st
    , onChange \e -> do
        target <- liftEffect $ Event.target e
        embed $ modify \_ -> (unsafeCoerce target).value
        pure Nothing
    , onKeyDown \e -> do
        keyCode <- liftEffect $ Event.keyCode e
        if round keyCode == 13
          then if S.length st > 0
            then pure $ Just $ Input st
            else pure $ Just Delete
          else if round keyCode == 27
            then pure $ Just Cancel
            else pure Nothing
    , onBlur \_ -> pure $ Just Cancel
    ] []

checkbox :: Component Boolean
checkbox = stateCached \embed st -> input
  [ _type "checkbox"
  , className "todo-checkbox"
  , checked st
  , onChange \_ -> do
      embed $ modify not
      pure Nothing
  ] []

data Entered = Entered String

inputOnEnter :: ∀ s. FocusedComponent s String Entered
inputOnEnter = stateCached \embed str -> input
  [ className "todo-input"
  , placeholder "What needs to be done?"
  , autoFocus true
  , value str
  , onChange \e -> do
      target <- liftEffect $ Event.target e
      modify \_ -> (unsafeCoerce target).value
      pure Nothing
  , onEnter $ if S.length str > 0
      then pure $ Just $ Entered str
      else pure Nothing
  ] []

data DeleteTodo = DeleteTodo

todoInput :: ∀ s.
  FocusedComponent s
     { temp :: String
     , current :: String
     , active :: Boolean
     }
   DeleteTodo
todoInput = stateCached \embed st -> if st.active
  then zoom _temp blurableInput \result -> do
    case result of
      Just Cancel -> do
        embed $ modify \st' -> st' { temp = "", active = false }
        pure Nothing
      Just (Input str) -> do
        embed $ modify \st' -> st' { temp = "", active = false, current = st.temp }
        pure Nothing
      Just Delete -> pure $ Just DeleteTodo
      _ -> pure Nothing
  else label
    [ className "todo-description"
    , onDoubleClick \_ -> do
        embed $ modify \st' -> st' { temp = st.current, active = true }
        pure Nothing
    ]
    [ text st.current ]

data Clicked = Clicked

spanButton :: ∀ s t. Array (FocusedComponent s t Unit) -> FocusedComponent s t Clicked
spanButton children = span [ onClick \_ -> pure $ Just Clicked ] (map ignore children)

todo
  :: ∀ s.
     Int                                -- | Todo id
  -> FocusedComponent s ToDo DeleteTodo -- | Todo Component
todo = stateCached2 \_ _ _ -> div
  [ className "todo" ]
  [ zoom _completed checkbox (const $ pure Nothing)
  , flip zoom todoInput
      { temp: _input
      , current: _description
      , active: _edited
      }
      pure
  , div [ className "todo-delete", onClick \_ -> pure $ Just DeleteTodo ] []
  ]

todoMVC :: ∀ s. FocusedComponent s AppState Unit
todoMVC = state \st embed -> div [ className "container" ]
  -- Input field
  [ zoom _todo inputOnEnter \str -> case str of
      Just (Entered str) -> do
        embed $ modify \st' -> st'
          { todo = ""
          , nextId = st'.nextId + 1
          , todos = M.insert st'.nextId
              { description: str
              , completed: false
              , edited: false
              , input: ""
              } st.todos
          }
        pure Nothing
      _ -> pure Nothing

  -- Individual todos
--  , zoom _todos $ foreachMap show compare ((invert ○ _) ○ (compare `on` fst)) (visible st.filter ○ snd) todo

  -- Footer
  , div
      [ className "footer" ]
      [ span
          [ className "todo-count"]
          [ text $ show (length $ filter (not _.completed) $ M.values st.todos) <> " items left" ]

      -- , div
      --     [ className "todo-filters" ]
      --     [ spanButton (embed $ modify $ set _filter All) [ text "All" ], text "/"
      --     , spanButton (embed $ modify $ set _filter Active) [ text "Active" ], text "/"
      --     , spanButton (embed $ modify $ set _filter Completed) [ text "Completed" ]
      --     ]

      , if (length $ filter (_.completed) $ M.values st.todos) > 0
          then span
            [ className "todo-clear"
            , onClick \_ -> do
                embed $ modify \st' -> st' { todos = filterMap (not (\todo -> todo.completed)) st.todos }
                pure Nothing
            ]
            [ text "Clear completed"]
          else span [] []
      ]
  ]
  where
    visible :: ToDoFilter -> ToDo -> Boolean
    visible All _ = true
    visible Active todo' = not todo'.completed
    visible Completed todo' = todo'.completed

    filterMap :: ∀ k v. Ord k => (v -> Boolean) -> Map k v -> Map k v
    filterMap f = M.fromFoldable ○ filter (f ○ snd) ○ M.toUnfoldable

-- Main ------------------------------------------------------------------------

main_ :: AppState -> (AppState -> E.Effect Unit) -> E.Effect Unit
main_ = run "main" todoMVC

main :: E.Effect Unit
main = run "main" todoMVC initialState (\_ -> pure unit)
