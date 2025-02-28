# Types

---

### Types

* Every expression has a type
* Haskell can deduce the type by its own
* Use `:t` in `ghci` to see the type

---

### Basic types

* Types = set of constants.
* Basic types: `Char`, `Int`, `Integer`, `Float`, `Double`, `Bool`.

```haskell
Bool = {True, False}
Char = {'a', 'b', 'c', ...}
Integer = {..., -1, 0, 1, 2, ...} -- potentially infinite
Int = { -2^29, ... 2^29-1 } -- depends on system, efficient
```

---

### Functional types

* Functions also have types!
```haskell
>> :t (&&)
(&&) :: Bool -> Bool -> Bool
```
* Function with two parameters = function with one parameter, 
returning a function
```Haskell 
 a -> b -> c === a -> (b -> c)
 a -> b -> c -> d === a -> (b -> (c -> d))
```

---

### Partial application

* Any function with `n` parameters can be applied partially 

```haskell
function :: a -> b -> c -> d

function x :: b -> c -> d

function x y :: c -> d

function x y z :: d
```

---

### Operator sectioning

* Syntactic sugar for operators
* `(x ***)` = `\y -> x *** y`
* `(*** x)` = `\y -> y *** x`


* `(-x)`  is a number! `(x-)` is a function.

---

### Typeclasses

```haskell
>> :t 3
3 :: Num p => p
```

* `3` is a *polymorphic* constant. 
* `Num p => ` -- context.
* `Num` -- typeclass (~ traits in Rust or interfaces).
  * Haskell does not know the exact type yet.
  * But knows the constraint.
* Use `:: Type` to specify type explicitly
* No implicit type conversions 

---

### Basic typeclasses

```Text
* Eq:         ==, /= 
* Ord:        <, >, <=, >= 
* Num:        +, -, *, signum, fromInteger 
* Fractional: /  
* Integral:   div 
* Show:       show (= to string) 
```
```haskell
Fractional ⊂ Num
Integral ⊂ Num
Ord ⊂ Eq
```

---

### Polymorphism

Two types of polymorphism:
  * `parametric`: implementation is independent on the type
  * `ad-hoc`: separate implementation for each type

---

### Parametric polymorphism

```haskell
id :: a -> a 
const :: a -> b -> a
flip :: (a -> b -> c) -> (b -> a -> c)
```
```haskell
undefined :: a
```
```haskell
map :: (a -> b) -> [a] -> [b]
filter :: (a -> Bool) -> [a] -> [a]
head :: [a] -> a
```
```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```
```haskell
($) :: (a -> b) -> a -> b
```

```haskell
curry :: ((a, b) -> c) -> a -> b -> c
uncurry :: (a -> b -> c) -> (a, b) -> c
```

---

#### Operators `($)` and `(.)`
* Low-priority function application `($)`
```haskell
infixr 0 $
($) :: (a -> b) -> a -> b
($) f x = f x
```
* Function composition `(.)`
```haskell
infixr 9 .
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x) 
```

---

### Ad-hoc polymorphism

* Implementation depends on the type

```Haskell
>> :t (+)
(+) :: Num a => a -> a -> a

>> :t (/=)
(/=) :: Eq a => a -> a -> Bool

>> :t (>)
(>) :: Ord a => a -> a -> Bool

>> :t show
show :: Show a => a -> String
```

* `Num a`, `Eq a`, `Show a` -- context
* "`a` must be an *instance* of of *typeclass* `Num` (`Eq`, `Show`)"

---

### Maybe
```haskell
data Maybe a = Just a | Nothing
```
* `Maybe a` ~ `Optional<a>` in Java. 
* Has a type parameter: `Maybe Bool` is a type, `Maybe` is not
* Values are `(Just x)` or `Nothing`

```haskell
Maybe Bool = {
  Just True, 
  Just False, 
  Nothing :: Maybe Bool
}
```
* `Nothing` is different for different `a`
* Use pattern matching!
---
### Either

```haskell
data Either a b = Left a | Right b
```
* 2 type parameters! 
* `Either String Integer` is a type
* Values are `(Left x)` or `(Right x)`

```haskell
Either Bool Char = {
  Left True, 
  Left False,
  Right 'a',
  Right 'b',
  Right 'c',
  ...
}
```