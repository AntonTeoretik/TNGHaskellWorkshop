# Custom types
---

### General pattern
* Type = set.
* Define type = define **data constructors**.
* Use `data` keyword

```Haskell
data MyType  = Con1 T_1_1 ... T_1_m1            
               | Con2 T_2_1 ... T_1_m2
               ...
               | Conn T_n_1 ... T_n_mn
```

---

### Enum (sum type)
```Haskell
data MyType = Con1 | Con2 | Con3 | ... | Con_n
```
* Contains exactly `n` values
* `Con1`, `Con2`, ... -- data constructors
* Use patten matching!

---

### Product type
```haskell
data MyType = Con T1 T2 .. Tn
```

* `Con` -- data constructor (often = `MyType`)
* `|MyType|` = `|T1|`×`|T2|`× ... ×`|Tn|`
* Elements of `MyType` are of the form 
```haskell
Con x1 x2 ... xn
```
* `Con` is actually a function:
```haskell
Con :: T1 -> T2 -> ... -> Tn -> MyType
```
---

### Record syntax
* Named "getters" or "projections" of your structure

```haskell
data MyType = Con {
  p_name_1 :: T1, 
  p_name_2 :: T2,
  ... 
  p_name_n :: Tn
}
```
```haskell
p_name_i :: MyType -> Ti
```
* Useful for pattern matching

```Haskell
myFunc :: MyType -> a
myFunc (Con {p_name_i = ..., p_name_j = ...}) = ...
```
* Or creating new value from the old one

```haskell
update :: MyType -> Ti -> MyType
update p new_value = p {p_name_i = new_value}
```

---

### Sum of products
```Haskell
data MyType = Con_1 T_1_1 ... T_1_m1            
            | Con_2 T_2_1 ... T_1_m2
            ...
            | Con_n T_n_1 ... T_n_mn
```
* `Con1`, `Con2`, ... -- data constructors

```haskell
Con_i :: T_i_1 -> T_i_2 -> ... -> T_i_mi -> MyType
```
```haskell
|MyType| = |T_1_1|×...×|T_1_m1|+ ... + |T_n_1|×...×|T_n_mn|
```
* Some of `T_i_j` may be equal to `MyType`: recursive types!

---

### Instance of typeclass
```haskell
instance Class Type where
  method1 = ...
  method2 = ...
```

* enough to implement only methods from 
```
{-# MINIMAL ... #-}
```
* use `deriving` for simple classes: `Eq`, `Show`, `Ord`

---

### Infix data constructor

```Haskell 
infixr n :++++
data MyType = (:++++) T1 T2 .. TN
            | ...
```
* Must start with `:` . 
* For usual lists `:` IS the infix data constructor.

---

### Parametric types
```Haskell
data MyType a b ... z = 
                 Con1 T_1_1 ... T_1_m1            
               | Con2 T_2_1 ... T_1_m2
               ...
               | Con_n T_n_1 ... T_n_mn 
-- T_i_j` are `a`-`z` (or concrete types)
```
* `MyType` is a *type constructor*
* `MyType T1 ... Tn` is a type
* Parameters can be partially applied!
  * `Either String` is 1-parametric type 
* Use `:k` to get its *kind*
```Haskell
>> :k MyType
MyType :: * -> * -> ... -> *
```

---

### Instances for parametric types

```haskell
data MyType a b ... z = ...

instance (constrains on a...z) => 
  Class (MyType a b ... z) where
  method1 = ...
  method2 = ...
```

where constrains on a...z are of the form

```haskell
(Class1 i1, Class2 i2, ...)
```

---

### Important parametric types
```haskell
Maybe :: * -> *
Either :: * -> * -> *
```
```haskell
[] :: * -> *
```
```haskell
(,) :: * -> * -> *
(,,) :: * -> * -> * -> *
...
```
* `(->)` is a type constructor!

```Haskell
>> :k (->)
(->) :: * -> * -> *
```
```Haskell
>> :i (->)
data (->) (a :: TYPE q) (b :: TYPE r) -- old version
```

---

### `type` and `newtype`
* `type` introduces a synonym for a type
  * Same data constructors

```Haskell
type T1 = T

f :: T1 -> T
f = id -- ok
```

* `newtype` is only for
  * types with one constructor
  * with exactly one parameter
  * zero cost abstraction
  
```haskell
newtype T1 = T1 T

f :: T1 -> T
f = id -- error
```


