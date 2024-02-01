# Monads

---

### Kleisli arrows 

* Functions `f :: a -> m b`.
* Can be viewed as functions with effect

```Haskell
f :: a -> Maybe b      -- partially defined
f :: a -> [b]          -- multivalued
f :: a -> (Either s) b -- can return typed "exception"
f :: a -> (s, b)       -- write additional info
f :: a -> ((->) e) b   -- read something from environment
f :: a -> IO b         -- communicate with real world
```
---

### Effect accumulation

```Hasklell 
(>>=) :: m a -> (a -> m b) -> m b
```
* `ma :: m a` -- value with effect
* `f :: a -> m b` -- function with effect
* `ma >>= f` -- extract value, apply function, combine effects

---
### Effect Accumulation example

```Haskell
safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

safeInverse :: Double -> Maybe Double
safeInverse 0 = Nothing
safeInverse x = Just $ 1 / x
```

```Haskell
>> Just [] >>= safeHead >>= safeInverse
Nothing
>> Just [0.0] >>= safeHead >>= safeInverse
Nothing
>> Just [4.0, 0.0] >>= safeHead >>= safeInverse
Just 0.25
```

---

### Definition
```Haskell
type Monad :: (* -> *) -> Constraint
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  {-# MINIMAL (>>=) #-}
-- return can be derived from Applicative  
```

```Haskell
return -- trivial way to pack value into container
ma >> mb = ma >>= \_ -> mb -- forget ma, but preserve the effect
```

---

#### `Maybe` as monad

```Haskell
instance  Monad Maybe  where
    (Just x) >>= k = k x
    Nothing  >>= _ = Nothing
    
    return = Just
```
```Haskell 
>> Just [] >>= safeHead >>= safeInverse
Nothing
>> Just [0.0] >>= safeHead >>= safeInverse
Nothing
>> Just [4.0, 0.0] >>= safeHead >>= safeInverse
Just 0.25
```

---

#### `List` as monad
```Haskell
instance Monad []  where
    [] >>= _ = []
    (x : xs) >>= f = f x ++ (xs >>= f)
            
    return a = [a]
```
```Haskell
foo :: Integer -> [Integer]
foo x = [2*x, x]

bar :: Integer -> [Integer]  
bar x = [x-1, x, x+1]
```
```Haskell
>> (return 10) >>= foo >>= bar
[19,20,21,9,10,11]
```
---

#### `Either a` as monad
```Haskell
instance Monad (Either e) where
    Left  l >>= _ = Left l
    Right r >>= k = k r
    
    return = Right
```
```Haskell
safeInv :: Double -> Either String Double
safeInv 0 = Left "Division by zero!"
safeInv x = Right 1 / x

safeSqrt :: Double -> Either String Double
safeSqrt x | x >= 0 = Right $ sqrt x
           | otherwise = Left "Sqrt of negative!"
```
```Haskell
>> (return 0) >>= safeInv >>= safeSqrt
Left "Division by zero!"
>> (return (-0.5)) >>= safeInv >>= safeSqrt
Left "Sqrt of negative!"
>> (return 4) >>= safeInv >>= safeSqrt
Right 0.5
```

---

#### `(r -> )` as monad (Reader)

```Haskell
instance Monad ((->) r) where
  (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
  read >>= f = \r -> f (read r) r
  
  return :: a -> (r -> a)
  return a = \r -> a
```

```Haskell
showVal :: String -> Int -> String
showVal str x = str ++ "Value: " ++ show x

showDoubled :: String -> Int -> String
showDoubled str x = str ++ ", and doubled is " ++ show (x * 2)
```
```Haskell
>>  (return "") >>= showVal >>= showDoubled $ 6
"Value: 6, and doubled is 12"
>>  (return "Hi! ") >>= showVal >>= showDoubled $ 6
"Hi! Value: 6, and doubled is 12"
```
---

#### `(,) a` as monad (Writer)

```Haskell
instance Monoid a => Monad ((,) a) where
  (u, a) >>= k = case k a of (v, b) -> (u <> v, b)
  return a = (mempty, a)
```
```Haskell
addFive :: Integer -> (String, Integer)
addFive x = ("Added 5; ", x + 5)

isPositive :: Integer -> (String, Bool)
isPositive x = ("Checked positivity; ", x > 0)
```
```Haskell
>> return 5 :: (String, Int)
("",5)
>> ( return (-1) ) >>= addFive >>= isPositive  
("Added 5; Checked positivity; ",True)
```

---

### Do-notation
```Haskell
hugeComputation = let i = 5 in
  addFive i     >>= \x ->  -- x := i + 5 ;
  addFive x     >>= \y ->  -- y := x + 2 ;
  isPositive  y >>= \z ->  -- z = (y > 0) ;
  return (x, y, z)         -- return (x, y, z)
```
```Haskell
>> hugeComputation
("Added 5; Added 5; Checked positivity; ",(10,15,True))
```
```text
e1 >>= \p -> e2   ---> do {p <- e1; e2}  
let v = e1 in do  ---> do {let v = e1; e2}
e1 >> e2          ---> do {e1; e2} 
```
--- 
```Haskell
hugeComputation' = do
  let i = 5
  x <- addFive i
  y <- addFive x
  z <- isPositive y
  return (x, y, z)
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

---

### `IO`
```Haskell
newtype IO a = IO (RealWorld -> (RealWorld, a))
```

* `RealWorld` is a "purely magical" type
* Represents the state of real world
* -> we can communicate with it

```Haskell
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
  putStrLn "Now some text will be printed"
  currentDir <- getCurrentDirectory
  line <- readFile $ currentDir ++ "\\test.txt"
  putStrLn ("Text in file: " ++ line)
```
