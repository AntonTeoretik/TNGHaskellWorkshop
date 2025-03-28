# Applicative

---

### Problem

* Functors "lift" functions of one argument
```Haskell
fmap   :: (a -> b)      -> (f a -> f b)
```
* Can we lift functions with multiple parameters?
```Haskell
liftA2 :: (a -> b -> c) -> (f a -> f b -> f c)
```
* Just `fmap` does not work:
```Haskell
fmap :: (a -> (b -> c)) -> f a -> f (b -> c)
```

* Can we transform 
```Haskell 
(<*>) :: f (b -> c) -> (f b -> f c)
``` 
  *naturally*?

---
### Applicative

```Haskell
class Functor f => Applicative f where
  pure   :: a -> f a -- must pack value in "obvious" way

  (<*>)  :: f (a -> b) -> (f a -> f b) -- infixl 4
  
  liftA2 :: (a -> b -> c) -> (f a -> f b -> f c)

  -- + some convenience methods 
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
```
* We need some axioms here

---
### Applicative
```Haskell
class Functor f => Applicative f where
  pure   :: a -> f a

  (<*>)  :: f (a -> b) -> (f a -> f b) -- infixl 4
  
  liftA2 :: (a -> b -> c) -> (f a -> f b -> f c)

  -- + some convenience methods 
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
```
* Define `<*>` => define `liftA2`

```Haskell
liftA2 f ax ay = f <$> ax <*> ay
liftA2         = (.) (<*>) . (<$>)
```
* Define `liftA2` => define `<*>`

```Haskell
(<*>) = liftA2 id
```
---

### `Maybe` is Applicative
```Haskell
instance Applicative Maybe where
    pure = Just

    Just f  <*> Just x   = Just (f x)
    _       <*> _        = Nothing

    liftA2 f (Just x) (Just y) = Just (f x y)
    liftA2 _ _         _       = Nothing
```

* "Safe" execution -- if anything is `Nothing`, result is `Nothing`
* `<*>` can be used for multiple parameters

```Haskell
f a b c d = a + b + c + d

>> Just f <*> Just 1 <*> Just 2 <*> Just 3 <*> Just 4
-- Just 10
```

---
### `Either e` is Applicative
```Haskell
instance Applicative (Either e) where
    pure          = Right
    Left  e <*> _ = Left e
    Right f <*> r = f <$> r
```
* Stores the first error message in execution

---
### `[]` is Applicative
```Haskell
instance Applicative [] where
    pure x         = [x]
    fs <*> xs      = [f x | f <- fs, x <- xs]
    liftA2 f xs ys = [f x y | x <- xs, y <- ys] 
  -- list comprehension

  -- | fs <*> xs <*> yx | = |fs| * |xs| * |yx|
```

* Applies all functions to all combination of parameters

---
### `(,) a` is Applicative
but only if `a` is _Monoid_
```Haskell
class Monoid a where -- old way
  (<>)   :: a -> a -> a -- must be associative
  mempty :: a -- must be identity for (<>)
--- prime example -- lists, (<>) is (++)
```
```Haskell
instance Monoid a => Applicative ((,) a) where
  pure x                 = (mempty, x)
  (u, f) <*> (v, x)      = (u <> v, f x)
  liftA2 f (u, x) (v, y) = (u <> v, f x y)
```
* Stores "logs"

---
### `(->) e` is Applicative

```Haskell
instance Applicative ((->) e) where
    pure :: a -> (e -> a)
    pure  = const
    
    (<*>)    :: (e -> (a -> b)) -> (e -> a) -> e -> b 
    (<*>) f g = \e -> (f e) (g e)
    
    liftA2 :: (a -> b -> c) -> (e -> a) -> (e -> b) -> (e -> c)
    liftA2 r_f r_a r_b = \e -> r_f (r_a r) (r_b r)
```
```md
`e` is _context_ or _data source_

`r_a :: e -> a` = read `a` from source `e`

`(r_f <*> r_a) e` = 
  read function `f`, read value `a` and apply `f` to `a`
```

---

### Applicative laws

```Haskell
-- Identity
 pure id <*> fa = fa
```
```Haskell
-- Composition
 pure (.) <*> ff <*> fg <*> fa = ff <*> (fg <*> fa)
```
```Haskell
-- Homomorphism
 pure f <*> pure a = pure (f a)
```
```Haskell
-- Interchange
 ff <*> pure a = pure ($ a) <*> ff
```




