# Monads

---

### Kleisli arrows 

* Functions `f :: a -> m b`.
* Can be viewed as functions with effect

```Haskell
f :: a -> Maybe b      -- partially defined
f :: a -> [b]          -- multi-valued
f :: a -> (Either s) b -- can return typed "exception"
f :: a -> (s, b)       -- write additional info
f :: a -> ((->) e) b   -- read something from environment
f :: a -> IO b         -- communicate with real world
```
---

### Problem
* Can we "lift" those functions

```Haskell
???  :: (a -> m b) -> (m a -> m b)
```
* Compare to

```Haskell
fmap ::   (a -> b) -> (f a -> f b)
<*>  :: f (a -> b) -> (f a -> f b)
```

* New operator: `>>=`

---

### Effect accumulation

```Haskell 
(>>=) :: m a -> (a -> m b) -> m b
```
* `ma :: m a` -- value with effect
* `f :: a -> m b` -- function, producing effect
* `ma >>= f` -- extract value, apply function, combine effects

```Haskell
:t flip (>>=)
(a -> m b) -> (m a -> m b) 
```

---

### Monad
```Haskell
type Monad :: (* -> *) -> Constraint
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b -- binding
  (>>) :: m a -> m b -> m b -- lighted binging
  return :: a -> m a
  {-# MINIMAL (>>=) #-}
```
* We need some axioms

```Haskell
return = pure
-- trivial way to pack value into container
ma >> mb = ma >>= const mb 
-- forget ma, but preserve the effect
```

---

#### `Maybe` as monad

```Haskell
instance  Monad Maybe  where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    (Just x) >>= f = f x 
    -- if there is a value, take it and apply
    Nothing  >>= _ = Nothing 
    
    return = Just
```

* Safe execution
* `>>=` is ~ flatmap for `Optional<T>` 

---

#### `[]` as monad
```Haskell
instance Monad []  where
    (>>=) :: [a] -> (a -> [b]) -> [b]
    []       >>= _ = []
    (x : xs) >>= f = f x ++ (xs >>= f)
            
    return a = [a]
```
* Similar as `flatMap` for `Stream<T>` in Java
---

#### `Either a` as monad
```Haskell
instance Monad (Either e) where
    (>>=) :: Either e a -> (a -> Either e b) -> Either e b

    Left  error >>= _ = Left error
    Right r     >>= f = f r
    
    return = Right
```
* If there is an error -- keep it
* If not -- take value, apply function
* Similar to `try-catch`

---


### `(,) a` as monad (Writer)

```Haskell
instance Monoid d => Monad ((,) d) where
  (>>=) :: (d, a) -> (a -> (d, b)) -> (d, b)
  (old_message, a) >>= k = 
    let (new_message, b) = k a in 
      (old_message <> new_message, b)
  return a = (mempty, a)
```

* Each action adds additional information
---

#### `(r -> )` as monad (Reader)

```Haskell
instance Monad ((->) r) where
  (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
  read >>= f = \r -> f (read r) r
  
  return :: a -> (r -> a)
  return a = \r -> a
```
* r is data source (given after computation is defined)
* `(a -> r -> b)` -- do something with `a`, in the context `r`
* `fa >>= k` -- read `a` from the source and apply `k`

---

### Do-notation
* Convenient syntax sugar for monadic computations

```Haskell
-- e1 >>= \p -> e2
do 
  p <- e1
  e2
```
```Haskell
-- let v = e1 in e2
do 
  let v = e1
  e2
```
```Haskell
--- e1 >> e2
do
  e1
  e2
```

---

### `s -> (s, _)` as monad (State)
```Haskell
f :: a -> m b -- generic Kleisli arrow
f :: a -> s -> (s, b) -- effect = read state and return new one
```
```Haskell
return :: a -> s -> (e, a)
return a s = (e, a)
```
```Haskell
(>>=) :: (s -> (e, a)) -> (a -> s -> (e, b)) -> (s -> (e, b))
(>>=) fa k s = k a s' 
  where
    (s', a) = fa s
```

---

### `State`
```Haskell
newtype State s a = State {runState :: s -> (s, a)}

instance Monad (State s) where
--return :: a -> State s a
  return a = State $ \s -> (s, a)
  
--(>>=) :: (State s a) -> (a -> State s b) -> (State s b)
  (>>=) sa k = State $ fb' where
    fb' s = (s'', b) where
      (s'', b) = fb s'
      fb = runState $ k a 
      (s', a) = runState sa $ s
```
* State = Read + Write

---

### `IO`
```Haskell
newtype IO a = IO (RealWorld -> (RealWorld, a))
```

* `RealWorld` is a "purely magical" type
* Represents the state of real world
* -> we can communicate with it!

```Haskell
putStrLn :: String -> IO ()
getLine :: IO String
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()
...
```

* `IO ()` -- "pure" side effect
* `main :: IO ()`

