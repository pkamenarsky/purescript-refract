## Module Refract.Lens

#### `ListToLens`

``` purescript
class ListToLens st (l :: RowList) (r :: RowList) | l st -> r, r st -> l
```

##### Instances
``` purescript
ListToLens st Nil Nil
(Strong p, ListToLens st ls rs) => ListToLens st (Cons k (p o o -> p st st) ls) (Cons k o rs)
```

#### `RecordToLens`

``` purescript
class RecordToLens st (r :: # Type) (rs :: # Type) 
```

##### Instances
``` purescript
(ListToRow rl r, ListToRow rsl rs, RowToList r rl, RowToList rs rsl, ListToLens st rl rsl) => RecordToLens st r rs
```

#### `recordToLens`

``` purescript
recordToLens :: forall r rs st. RecordToLens st r rs => {  | r } -> Lens' st ({  | rs })
```


