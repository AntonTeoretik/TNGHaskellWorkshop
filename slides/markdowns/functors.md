## Functor

---

### Functor

```Haskell
type Functor :: (* -> *) -> Constraint
class Functor t where
  fmap :: (a -> b) -> (t a -> t b)
```
```Haskell
-- plus axioms!
fmap id = id
fmap (f . g) = fmap f . fmap g
```

* `t` must be a 1-parametric type (`* -> *`)
* Compiler can't check if axioms are fulfilled...
* ... but uses them for optimization!

---

### Some instances of functor

```Haskell
instance Functor [] -- Defined in `GHC.Base'
instance Functor Maybe -- Defined in `GHC.Base'
instance Functor ((,) a) -- Defined in `GHC.Base'
instance Functor ((->) r) -- Defined in `GHC.Base'
instance Functor (Either a) -- Defined in `Data.Either'
instance Functor IO -- Defined in `GHC.Base'
```

---

### `[]` is a functor
```Haskell
>> fmap (==3) [1, 2, 3]
[False,False,True]

>> fmap (+3) [1, 2, 3]
[4,5,6]
```
```Haskell
fmap :: (a -> b) -> [a] -> [b]
```

* Same as `map`!

```Haskell
instance Functor [] where
    {-# INLINE fmap #-}
    fmap = map
```
```Haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

---

### `Maybe` is a functor

```Haskell
>> fmap (+3) $ Just 3
Just 6

>> fmap (+3) $ Nothing
Nothing
```
```Haskell
fmap :: (a -> b) -> Maybe a -> Maybe b
```
```Haskell
instance  Functor Maybe  where
    fmap _ Nothing  = Nothing
    fmap f (Just a) = Just (f a)
```

---

### `(,) a` is a functor

```Haskell
>> fmap (+3) ("Something", 4)
("Something", 7)
```
```Haskell
fmap :: (b -> c) -> (a, b) -> (a, c)
```
```Haskell
instance Functor ((,) a) where
  fmap f (x,y) = (x, f y)
```

### `(,,) a b` is a functor

```Haskell
>> fmap (+3) ("Something", True, 4)
("Something", True, 7)
```
```Haskell
instance Functor ((,,) a b) where
  fmap f (x,y,z) = (x,y,f z)
```

---

### `Either e` is a functor

```Haskell
>> fmap (+3) $ Left "Something"
Left "Something"

>> fmap (+3) $ Right 5
Right 8
```
```Haskell
fmap :: (a -> b) -> Either e a -> Either e b
```
```Haskell
instance Functor (Either e) where
  fmap f (Left x) = Left x
  fmap f (Right x) = Right (f x)
```

---

### `r -> ` is a functor
```Haskell
fmap :: (a -> b) -> (r -> a) -> (r -> b)
```
```Haskell
instance Functor ((->) r) where
  fmap = (.)
```