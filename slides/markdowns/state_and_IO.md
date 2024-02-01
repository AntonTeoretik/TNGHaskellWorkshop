# Stats and IO

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
main :: IO ()
main = do
  putStrLn "Please enter a line of text:"
  currentDir <- getCurrentDirectory
  line <- readFile $ currentDir ++ "\\test.txt"
  putStrLn ("You entered: " ++ line)
```
