# Functor

---

### Functor

```Haskell
type Functor :: (* -> *) -> Constraint
class Functor t where
  fmap :: (a -> b) -> (t a -> t b)
```
```Haskell
-- plus axioms!
1. fmap id = id
2. fmap (f . g) = fmap f . fmap g
```

* `t` must be a 1-parametric type (`* -> *`)
* Compiler can't check if axioms are fulfilled...
* ... but uses them for optimization!
* `(<$>)` == `fmap` 

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
instance Functor [] where
    {-# INLINE fmap #-}
    fmap = map
```
```Haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

### `Maybe` is a functor

```Haskell
instance  Functor Maybe  where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing  = Nothing
    fmap f (Just a) = Just (f a)
```

---

### `(,) a` is a functor
```Haskell
instance Functor ((,) a) where
  fmap :: (b -> c) -> (a, b) -> (a, c)
  fmap f (x,y) = (x, f y)
```

### `(,,) a b` is a functor

```Haskell
instance Functor ((,,) a b) where
  fmap :: (c -> d) -> (a, b, c) -> (a, b, d)
  fmap f (x,y,z) = (x,y,f z)
```
---

### `Either e` is a functor

```Haskell
instance Functor (Either e) where
  fmap :: (a -> b) -> Either e a -> Either e b
  fmap f (Left x) = Left x
  fmap f (Right x) = Right (f x)
```

### `r -> ` is a functor
```Haskell
instance Functor ((->) r) where
  fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
  fmap = (.)
```
