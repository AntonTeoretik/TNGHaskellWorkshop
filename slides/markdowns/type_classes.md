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
  invert  :: a -> a
  neutral :: a

-- Group axioms:
-- x # y # z = (x # y) # z = x # (y # z)
-- neutral # x == x
-- x # neutral == x
-- x # (invert x) == neutral
-- (invert x) # x == neutral
```

* ~ to traits in Rust
* Compiler cannot check axioms :-(

---

### Instance

```Haskell
infixl 5 #

class Group a where
  (#) :: a -> a -> a
  invert  :: a -> a
  neutral :: a
```

```Haskell
instance Group Integer where
  neutral = 0
  invert = negate
  (#) = (+)
```
```Haskell
>> 1 # 4
5
>> invert 4
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
  invert  :: a -> a
  neutral :: a
```
```Haskell
instance Group () where
  neutral = ()
  invert = id
  () # () = ()
```
```Haskell
>> () # ()
()
>> invert ()
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
  invert  :: a -> a
  neutral :: a
```
```Haskell
instance (Group a, Group b) => Group (a, b) where
  (x1, y1) # (x2, y2) = (x1 # x2, y1 # y2)
  invert (x1, y1) = (invert x1, invert y1)
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
actually is in wrapper: `End`
```Haskell
newtype Endo a = Endo { appEndo :: a -> a }
```

