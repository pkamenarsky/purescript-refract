## Module Refract

#### `(○)`

``` purescript
infixr 9 compose as ○
```

A synonym for `<<<`.

#### `(×)`

``` purescript
infixr 6 Tuple as ×
```

A synonym for `Tuple`.

#### `type (×)`

``` purescript
infixr 6 type Tuple as ype (×
```

A type synonym for `Tuple`.

#### `_id`

``` purescript
_id :: forall a. Lens' a a
```

The identity `Lens`.

#### `EffectF`

``` purescript
data EffectF eff st next
```

The base functor of the `Effect` free monad.

##### Instances
``` purescript
Functor (EffectF eff st)
```

#### `Effect`

``` purescript
type Effect eff st = Free (EffectF eff st)
```

An `Effect` with base type `st`.

#### `modify`

``` purescript
modify :: forall eff st. (st -> st) -> Effect eff st Unit
```

Modify the current `Component` state.

#### `effectfully`

``` purescript
effectfully :: forall a eff st. (st -> Aff eff a) -> Effect eff st a
```

Perform a `Control.Monad.Aff` action and return a result.

#### `state`

``` purescript
state :: forall eff st. (st -> Component eff st) -> Component eff st
```

Reify the current `Component` state.

#### `stateL`

``` purescript
stateL :: forall eff st stt. ALens' st stt -> (stt -> Component eff st) -> Component eff st
```

Reify `Component` substate specified by a lens.

#### `stateR`

``` purescript
stateR :: forall eff r rs st. RecordToLens st r rs => {  | r } -> ({  | rs } -> Component eff st) -> Component eff st
```

`stateL` for generic records.

Specializing to a concrete state, its type would be:

```purescript
stateR
  :: ∀ eff st.
     { name :: ALens' st String
     , age :: ALens' st Int
     }
  -> ( { name :: String
       , age :: Int
       } -> Component eff st
     )
  -> Component eff st
```

#### `modifyL`

``` purescript
modifyL :: forall eff st stt. ALens' st stt -> (stt -> stt) -> Effect eff st Unit
```

Modify `Component` substate specified by a `Lens`.

#### `modifyR`

``` purescript
modifyR :: forall eff r rs st. RecordToLens st r rs => {  | r } -> ({  | rs } -> {  | rs }) -> Effect eff st Unit
```

`modifyL` for generic records.

Specializing to a concrete state, its type would be:

```purescript
modifyR
  :: ∀ eff st.
     { name :: ALens' st String
     , age :: ALens' st Int
     }
  -> ({ name :: String , age :: Int } -> { name :: String , age :: Int })
  -> Effect eff st Unit
```

#### `Unzoom`

``` purescript
type Unzoom eff st stt = Component eff st -> Component eff stt
```

#### `zoom`

``` purescript
zoom :: forall eff st stt. ALens' st stt -> Component eff stt -> Component eff st
```

Embed a `Component` with a state of type `stt` into a `Component`
with a state of type `st`.

#### `zoomUn`

``` purescript
zoomUn :: forall eff st stt. ALens' st stt -> (Unzoom eff st stt -> Component eff stt) -> Component eff st
```

Embed a `Component` with a state of type `stt` into a `Component`
with a state of type `st` and provide it with an unzoom combinator.

#### `zoomR`

``` purescript
zoomR :: forall eff r rs st. RecordToLens st r rs => {  | r } -> Component eff ({  | rs }) -> Component eff st
```

`zoom` for generic records.

Specializing to a concrete state, its type would be:

``` purescript
zoomR
  :: ∀ eff st.
     { name :: ALens' st String
     , age :: ALens' st Int
     }
  -> Component eff
       { name :: String
       , age :: Int
       }
  -> Component eff st
```

#### `zoomRUn`

``` purescript
zoomRUn :: forall eff r rs st. RecordToLens st r rs => {  | r } -> (Unzoom eff st ({  | rs }) -> Component eff ({  | rs })) -> Component eff st
```

`zoomUn` for generic records.

#### `Handler`

``` purescript
type Handler st = EventHandlerContext ReadWrite Unit st Unit
```

#### `Props`

``` purescript
type Props eff st = (Effect eff st Unit -> Handler st) -> Props
```

#### `Component`

``` purescript
type Component eff st = (Effect eff st Unit -> Handler st) -> st -> ReactElement
```

A `Component eff st` is parameterized over an effect type `eff` and a
state type `st` over which it operates.

#### `ComponentClass`

``` purescript
type ComponentClass eff st = ReactClass ((Effect eff st Unit -> Handler st) × st)
```

A React component class. Useful whenever a `Component` needs to implement
React lifecycle methods.

#### `Spec`

``` purescript
type Spec eff st = { displayName :: String, componentWillMount :: Effect eff st Unit, componentDidMount :: Effect eff st Unit, componentWillUnmount :: Effect eff st Unit, componentWillUpdate :: st -> Effect eff st Unit, componentDidUpdate :: st -> Effect eff st Unit, shouldComponentUpdate :: st -> Boolean }
```

React lifecycle spec.

#### `defaultSpec`

``` purescript
defaultSpec :: forall eff st. Spec eff st
```

No-op lifecycle method spec.

#### `componentClass`

``` purescript
componentClass :: forall eff st. Spec eff st -> Component eff st -> ComponentClass eff st
```

Create a `ComponentClass` from a `Spec` and a `Component`.

#### `component`

``` purescript
component :: forall eff st. ComponentClass eff st -> Component eff st
```

Create a `Component` from a `ComponentClass`.

#### `mkComponent`

``` purescript
mkComponent :: forall eff st. String -> Array (Props eff st) -> Array (Component eff st) -> Component eff st
```

Create a DOM element `Component`.

#### `run`

``` purescript
run :: forall eff st. String -> Component eff st -> st -> (st -> Eff eff Unit) -> Eff (dom :: DOM | eff) Unit
```

Attach `Component` to the DOM element with the specified id and run it.

#### `foreach`

``` purescript
foreach :: forall eff st a. ALens' st (Array a) -> (ALens' st a -> Component eff st) -> Component eff st
```

Unfiltered `Array` traversal.

#### `foreachF`

``` purescript
foreachF :: forall eff st a. (a -> Boolean) -> ALens' st (Array a) -> (ALens' st a -> Component eff st) -> Component eff st
```

Filtered `Array` traversal.

#### `foreachU`

``` purescript
foreachU :: forall eff st a. ALens' st (Array a) -> (Unzoom eff st a -> Component eff a) -> Component eff st
```

Unfiltered `Array` traversal providing an unzoom combinator.

#### `foreachUF`

``` purescript
foreachUF :: forall eff st a. (a -> Boolean) -> ALens' st (Array a) -> (Unzoom eff st a -> Component eff a) -> Component eff st
```

Filtered `Array` traversal providing an unzoom combinator.

#### `foreachZ`

``` purescript
foreachZ :: forall eff st a. ALens' st (Array a) -> Component eff a -> Component eff st
```

Zooming unfiltered `Array` traversal.

#### `foreachZF`

``` purescript
foreachZF :: forall eff st a. (a -> Boolean) -> ALens' st (Array a) -> Component eff a -> Component eff st
```

Zooming filtered `Array` traversal.

#### `foreachMap`

``` purescript
foreachMap :: forall eff st k v. Ord k => (k × v -> k × v -> Ordering) -> ALens' st (Map k v) -> (ALens' st v -> (st -> st) -> Component eff st) -> Component eff st
```

Unfiltered `Map` traversal.

#### `foreachMapF`

``` purescript
foreachMapF :: forall eff st k v. Ord k => (k × v -> k × v -> Ordering) -> (k × v -> Boolean) -> ALens' st (Map k v) -> (ALens' st v -> (st -> st) -> Component eff st) -> Component eff st
```

Filtered `Map` traversal.

#### `foreachMapU`

``` purescript
foreachMapU :: forall eff st k v. Ord k => (k × v -> k × v -> Ordering) -> ALens' st (Map k v) -> (Unzoom eff st v -> (st -> st) -> Component eff v) -> Component eff st
```

Unfiltered `Map` traversal providing an unzoom combinator.

#### `foreachMapUF`

``` purescript
foreachMapUF :: forall eff st k v. Ord k => (k × v -> k × v -> Ordering) -> (k × v -> Boolean) -> ALens' st (Map k v) -> (Unzoom eff st v -> (st -> st) -> Component eff v) -> Component eff st
```

Filtered `Map` traversal providing an unzoom combinator.


