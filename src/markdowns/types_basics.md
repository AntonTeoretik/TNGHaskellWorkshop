# Types

---

### Types

* Every expression has a type
* Haskell can deduce the type by its own

```haskell
>> :t 'c' 
'c' :: Char

>> :t True 
True :: Bool

>> :t 3 
3 :: Num p => p --- ?!
```

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

### Typeclasses

```haskell
>> :t 3
3 :: Num p => p
```

* `3` is a *polymorphic* constant. 
* `Num p => ` -- context.
* `Num` -- type class (~ interface in Java).
  * Haskell does not know the exact type.
  * But knows the constraint.


---

### Typeclasses

* `Num`: basic operations `+`, `-`, `*`, `signum`, `fromInteger`.
* `Fractional`: `/`.
* `Integral`: `div`.
* `Eq`: `==`, `/=`.
* `Ord`: `<`, `>`, `<=`, `>=`.
* `Show`: `show` (= to string).

```haskell
Fractional ⊂ Num
Integral ⊂ Num
Ord ⊂ Eq
```

---

### Typeclasses

```haskell
>> let x = 3 :: Integer
>> :t x 
x :: Integer

>> let y = 7 :: Double
>> :t y 
y :: Double

>> y
7.0
```
```haskell
>> x + 7 -- ok
>> x + y -- forbidden! (`+` takes parameters of the same type)
```

---

### Functional types

```haskell
>> :t not
not :: Bool -> Bool

>> :t (&&)
(&&) :: Bool -> Bool -> Bool
```
``` a -> b -> c === a -> (b -> c) ```

A function with two parameters is the functions with one parameter, 
which returns a function with one parameter

---

### Polymorphism

* Many functions are polymorphic
* Two types of polymorphism: parametric, ad-hoc

```Haskell
>> :t id
id :: a -> a

>> :t const
const :: a -> b -> a
```
```Haskell
>> :t (+)
(+) :: Num a => a -> a -> a

>> :t 3
3 :: Num p => p
```

---

### Parametric polymorphism

* Implementation does not depend on the parameter type.

```Haskell
id :: a -> a 
id x = x -- the only possible way to define such a function!
```
```Haskell
const :: a -> b -> a
const x _ = x
```
```Haskell
fst :: (a, b) -> a
fst (x, y) = x
```

Assume `f :: a`. How it can be defined?

---

#### Function application `$`

* Function application (` `) is left-associative and has the highest priority (`10`).

`f a b + c = ((f a) b) + c`
* Operator `$` does the same, but the lowest priority (`0`) and right-associative.

```haskell
>> :t ($)
($) :: (a -> b) -> a -> b
>> show $ max 10 $ min 0 5

"10"
-- same as show (max 10 (min 0 5))
```

---

#### Strict function application `$!`

* Same as `$`, but forces to evaluate expressions to WHNF.

```haskell
>> :t ($!)
($!) :: (a -> b) -> a -> b
>> show $! max 10 $! min 0 5

"10"
```

---

#### Function composition `.`

```haskell
infixr 9 .
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x) 
--- So it takes two function and returns the function!
```

```haskell
>> floor ( (+ 2) ( (min 5) 4.9))
6
>> floor $ (+ 2) $ (min 5) 4.9
6
>> floor . (+ 2) . min 5 $ 4.9
6
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
* "`a` must be of *type class* `Num` (`Eq`, `Show`)"

---

### Types inference

```Haskell
>> f x y = y + max x x
>> :t f
f :: (Num a, Ord a) => a -> a -> a
```
```Haskell
>> f' x y = y + max x x + (1 :: (Integral a => a))
>> :t f'
f' :: (Integral a) => a -> a -> a

-- class (Real a, Enum a) => Integral a
-- class (Num a, Ord a) => Real a
```

---

### Partial application

```haskell
>> andTrue = (&&) True
>> :t andTrue
andTrue :: Bool -> Bool
>> andTrue False
False

>> relu = max 0
>> :t relu
relu :: (Ord a, Num a) => a -> a
```

---

### Operator sectioning

Syntactic sugar

```haskell
>> square = (^2)
>> square 10
100

>> exp2 = (2^)
>> exp2 10
1024 
```

`(-1)`  is a number! `(1-)` is a function.








