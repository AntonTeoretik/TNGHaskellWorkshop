# Type classes

---

### Typeclasses

* Set of requirements on type.
* ~ interface.

```Haskell
>> :i Eq

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}

-- Defined in ‘GHC.Classes’
instance (Eq a, Eq b) => Eq (Either a b)
-- Defined in ‘Data.Either’
instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Maybe’
...
```

---

### Typeclasses

* Some class types requre some others!

```Haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
  ...
 
```

* If `a` "wants" to implement `Ord`, then it **must** implement `Eq`.


---

### Custom type class

```Haskell
infixl 5 #

class Group a where
  (#) :: a -> a -> a
  inverse  :: a -> a
  neutral :: a

-- Group axioms:
-- x # y # z = (x # y) # z = x # (y # z)
-- neutral # x == x
-- x # neutral == x
-- x # (inverse x) == neutral
-- (inverse x) # x == neutral
```

* ~ to traits in Rust
* Compiler cannot check axioms :-(

---

### Instance

```Haskell
infixl 5 #

class Group a where
  (#) :: a -> a -> a
  inverse  :: a -> a
  neutral :: a
```

```Haskell
instance Group Integer where
  neutral = 0
  inverse = negate
  (#) = (+)
```
```Haskell
>> 1 # 4
5
>> inverse 4
-4
>> neutral :: Integer
0
```

---

### Instance

```Haskell
infixl 5 #

class Group a where
  (#) :: a -> a -> a
  inverse  :: a -> a
  neutral :: a
```
```Haskell
instance Group () where
  neutral = ()
  inverse = id
  () # () = ()
```
```Haskell
>> () # ()
()
>> inverse ()
()
>> neutral :: ()
()
```

---

### Instance with context

```Haskell
infixl 5 #

class Group a where
  (#) :: a -> a -> a
  inverse  :: a -> a
  neutral :: a
```
```Haskell
instance (Group a, Group b) => Group (a, b) where
  (x1, y1) # (x2, y2) = (x1 # x2, y1 # y2)
  inverse (x1, y1) = (inverse x1, inverse y1)
  neutral = (neutral, neutral)
```
```Haskell
>> (1, 2) # (3, neutral)
(4, 2)
```

---

### Instance for existing type classes

```Haskell
data Complex = Complex Double Double deriving (Eq)

instance Show Complex where
  show (Complex x y) = ...
```
```Haskell
instance Num Complex where
  (+) (Complex x1 y1) (Complex x2 y2) = 
    Complex (x1 + x2) (y1 + y2)
  negate (Complex x y) = 
    Complex (-x) (-y)
  (*) (Complex x1 y1) (Complex x2 y2) = 
    Complex (x1 * x2 - y1 * y2) (x1 * y2 + x2* y1) 
  abs x = x 
  signum x = 1 
  fromInteger n = Complex (fromInteger n) 0 
```
```Haskell
>> Complex (-1) (-2) * Complex 2 (-1)
-4.0 - i*3.0
>> fromInteger 2 :: Complex 
2.0 + i*0.0
```

---

### Equations in type classes

```Haskell
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
  x == y = not (x /= y) 
  {-# MINIMAL (==) | (/=) #-}
```
```Haskell
instance Eq Bool where
  True == True = True
  False == False = True
  _ == _  = False 
```
* No need to implement all functions if they have default implementations
* `MINIMAL` is pragma

---

## Some important type classes

---

### Monoid
```Haskell
class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  {-# MINIMAL mempty | mconcat #-}
  
-- (<>) = mappend
```
Axioms
```Haskell
1.  x <> mempty = x
2.  mempty <> x = x
3.  x <> (y <> z) = (x <> y) <> z
```

---
### Examples

```Haskell
instance Monoid () where
  mempty   = ()
  () <> () = ()
```
```Haskell
instance Monoid [a] where
  mempty  = []
  mappend = (++)
```
```Haskell
instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing
  (Just x) <> (Just y) = Just (x <> y)
  x <> Nothing = x
  Nothing <> x = x
```
```Haskell
instance Monoid b => Monoid (a -> b) where
  mempty :: a -> b
  mempty _ = mempty
  
  (<>) :: (a -> b) -> (a -> b) -> (a -> b)
  f <> g = \a -> f a <> g a
```
---
### `a -> a` is a monoid
```Haskell
instance Monoid (a -> a) where
  mempty = id
  (<>) = (.)
```
needs wrapper: `Endo`
```Haskell
newtype Endo a = Endo { appEndo :: a -> a }
```

---
## Foldable

Allows to "fold" the whole structure into one element with given binary operation
```Haskell
class Foldable t where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap' :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  ...
  {-# MINIMAL foldMap | foldr #-}
  	-- Defined in `Data.Foldable'
```

---

### `[a]` as prime example
```Haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op val [] = val
foldr op val (x:xs) = x `op` (foldr val xs)
```
```Haskell
-- foldr op x [x1,...,xn] = 
--    x1 `op` (x2 ... `op` (xn `op` x)...)
```
```Haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op val [] = val
foldl op val (x:xs) = foldl (val `op` x) xs
```
```Haskell
-- foldl op x [x1,...,xn] = 
--     (... ((x `op` x1) `op` x2) ... ) `op` xn
```
```Haskell
>> foldl (+) 0 [0..10]
55
```

---
### `foldr` vs `foldl`
* `foldr`: can work with infinite lists 
  * initial value does not matter
  * can lead to stackoverflow
  * `foldr (:) [] xs == xs`
* `foldl`: rarely used
  * `foldl (flip (:)) [] xs == reverse xs`
* `foldl'`: strict version 
  * -> avoid stack overflow

---
### `foldr` use case

```Haskell
pickUntil0 = foldr pick [] where
  0 `pick` _ = []
  x `pick` acc = x : acc
```
```Haskell
>> pickUntil0 $ [1, 2, 3, 0] ++ [1..]
[1,2,3]
```

`foldl` will not work at all!
```Haskell
pickUntil0' = foldl pick [] where
  _ `pick` 0 = []
  acc `pick` x = x : acc
```
```Haskell
>> pickUntil0' $ [1, 2, 3]
[3,2,1]
>> pickUntil0' [1, 2, 3, 0, 4, 5, 6]
[6,5,4]
>> pickUntil0' [0..]
-- ...
```

---
### `foldl'` use case
```Haskell
>> foldl' max 0 [1..10^8]
100000000
```
```Haskell
>> foldr max 0 [1..10^8]
-- *** Exception: stack overflow (but rather fast)
```
```Haskell
>> foldl max 0 [1..10^8]
-- *** Exception: stack overflow (and slow)
```

---
### `foldl` use case
```Haskell
lazyMul :: Int -> Int -> Int
_ `lazyMul` 0 = 0
x `lazyMul` y = x * y
```
```Haskell
>> foldl lazyMul 1 [2,3,undefined,5,0,3]
0 
```
```Haskell
>> foldl' lazyMul 1 [2,3,undefined,5,0,3]
-- *** Exception: Prelude.undefined
```
---
### General foldable
* Behaves like a *stream*
* `toList`, `length`, `elem`, `maximum`, `and`, `any`, ...
* `Traversable`...

