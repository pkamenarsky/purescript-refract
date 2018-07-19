module ToDoMVC where
  
--------------------------------------------------------------------------------

import Refract

import Data.Array as A
import Data.Function (on)
import Data.Int (round)
import Data.Lens (ALens', Lens', _Just, cloneLens, over, set)
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
import Prelude (class Ord, Unit, bind, const, compare, discard, flip, identity, map, not, pure, show, when, unit, ($), (+), (<>), (==), (>), (*>))
import React.SyntheticEvent as Event
import Refract.DOM (div, input, label, span, text)
import Refract.Props (_type, autoFocus, checked, className, key, onBlur, onChange, onClick, onDoubleClick, onEnter, onKeyDown, placeholder, value)
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
  :: Component' {} String InputResult
blurableInput = state \_ st -> input
    [ className "todo-edit"
    , autoFocus true
    , value st
    , onChange \e -> do
        target <- liftEffect $ Event.target e
        modify \_ -> (unsafeCoerce target).value
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

checkbox :: Component {} Boolean
checkbox = state \_ st -> input
  [ _type "checkbox"
  , className "todo-checkbox"
  , checked st
  , onChange \_ -> do
      modify not
      pure Nothing
  ] []

data Entered = Entered String

inputOnEnter :: Component' { key :: String } String Entered
inputOnEnter = cache $ state \_ str -> input
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

data DeleteAction = DeleteAction

todoInput ::
  Component'
     {}
     { temp :: String
     , current :: String
     , active :: Boolean
     }
   DeleteAction
todoInput = state \_ st -> if st.active
  then zoom _temp blurableInput {} \result -> do
    case result of
      Just Cancel -> do
        modify \st' -> st' { temp = "", active = false }
        pure Nothing
      Just (Input str) -> do
        modify \st' -> st' { temp = "", active = false, current = st.temp }
        pure Nothing
      Just Delete -> pure $ Just DeleteAction
      _ -> pure Nothing
  else label
    [ className "todo-description"
    , onDoubleClick \_ -> do
        modify \st' -> st' { temp = st.current, active = true }
        pure Nothing
    ]
    [ text st.current ]

data Clicked = Clicked

spanButton :: ∀ s q. s -> Array (Component {} s) -> Component {} s
spanButton t children = state \_ _ -> span [ onClick \_ -> (modify $ const t) *> pure Nothing ] children

todo :: Component' { key :: String } ToDo DeleteAction
todo = cache $ state \props _ -> div
  [ className "todo" ]
  [ zoom _completed checkbox {} (const $ pure Nothing)
  , zoom 
      { temp: _input
      , current: _description
      , active: _edited
      }
      todoInput
      {}
      pure
  , div [ className "todo-delete", onClick \_ -> pure $ Just DeleteAction ] []
  ]

todos :: Component { key :: String, todoFilter :: ToDoFilter } (Map Int ToDo)
todos = cache $ state \{ todoFilter } st -> div [] $ flip map (todoArray todoFilter st) \(k × v) -> embed todo { key: show k } v (mod k) (delete k)
  where
    mod k v = modify \s -> M.insert k v s
    delete k DeleteAction = do
      modify \s -> M.delete k s
      pure Nothing

    todoArray :: ToDoFilter -> (Map Int ToDo) -> Array (Int × ToDo)
    todoArray todoFilter st
      = A.sortBy ((invert ○ _) ○ compare `on` fst)
      $ A.filter (visible todoFilter ○ snd)
      $ M.toUnfoldable st

    visible :: ToDoFilter -> ToDo -> Boolean
    visible All _ = true
    visible Active todo' = not todo'.completed
    visible Completed todo' = todo'.completed

todoMVC :: Component {} AppState
todoMVC = state \_ st -> div [ className "container" ]
  -- Input field
  [ zoom _todo inputOnEnter { key: "input" } \str -> case str of
      Just (Entered str') -> do
        modify \st' -> st'
          { todo = ""
          , nextId = st'.nextId + 1
          , todos = M.insert st'.nextId
              { description: str'
              , completed: false
              , edited: false
              , input: ""
              } st.todos
          }
        pure Nothing
      _ -> pure Nothing

  -- Individual todos
  , zoom _todos todos { key: "todos", todoFilter: st.filter } pure

  -- Footer
  , div
      [ className "footer" ]
      [ span
          [ className "todo-count"]
          [ text $ show (length $ filter (not _.completed) $ M.values st.todos) <> " items left" ]

      , div
          [ className "todo-filters" ]
          [ zoom _filter (spanButton All [ text "All" ]) {} (const $ pure Nothing), text "/"
          , zoom _filter (spanButton Active [ text "Active" ]) {} (const $ pure Nothing), text "/"
          , zoom _filter (spanButton Completed [ text "Completed" ]) {} (const $ pure Nothing)
          ]

      , if (length $ filter (_.completed) $ M.values st.todos) > 0
          then span
            [ className "todo-clear"
            , onClick \_ -> do
                modify \st' -> st' { todos = filterMap (not (\todo -> todo.completed)) st.todos }
                pure Nothing
            ]
            [ text "Clear completed"]
          else span [] []
      ]
  ]
  where
    filterMap :: ∀ k v. Ord k => (v -> Boolean) -> Map k v -> Map k v
    filterMap f = M.fromFoldable ○ filter (f ○ snd) ○ M.toUnfoldable

-- Main ------------------------------------------------------------------------

main_ :: AppState -> (AppState -> E.Effect Unit) -> E.Effect Unit
main_ = run "main" todoMVC

main :: E.Effect Unit
main = run "main" todoMVC initialState (\_ -> pure unit)
