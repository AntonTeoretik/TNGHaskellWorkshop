# Tuples and lists

---

### Tuples

```haskell
>> :t (True, 'c', not)
(True, 'c', not) :: (Bool, Char, Bool -> Bool)
```

* Store several values at once.
* Fixed length!
* No 1-tuples. `(4) == 4`.
* Unit type `()`: only one element -- empty tuple. 
```haskell 
>> :t ()
() :: () 
```

---

### Working with tuples

```haskell
f :: (Bool, Integer, Integer) -> Integer
f (True, a, b) = a + b
f (False, _, _) = 0

>> f (True, 5, 2)
7
>> f (False, 10, 3)
0
>> f (False, undefined, undefined)
0
-- here is no error because of laziness.
```
```haskell
>> :t fst
fst :: (a, b) -> a

>> :t snd
snd :: (a, b) -> b
```

---
### `@` in pattern matching
```Haskell
flipAndId :: (a, b) -> ( (a, b), (b, a) )
flipAndId p@(x, y) = (p, (y, x))
```
Can be used for other complex types
---

### Curry and uncurry
Function with 2 parameters ~ function of tuple
```Haskell
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y) 
```
```Haskell
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y 
```

---

### Lists

* Contain elements *of the same type*.

```haskell
>> :t [True, False]
[True, False] :: [Bool]
```
```haskell
>> :t ['H', 'e', 'l', 'l', 'o']
['H', 'e', 'l', 'l', 'o'] :: [Char] 
-- String = [Char] 
```
```haskell
>> :t [1, 2, 3]
[1, 2, 3] :: Num a => [a]
```
```haskell
>> :t []
[] :: [a]
```
```haskell
>> :t [[True], [False, True]]
[[True], [False, True]] :: [[Bool]]
```

---

### Ranges
```haskell
>> [1..10]
[1,2,3,4,5,6,7,8,9,10]
```
```haskell
>> [1,3..10]
[1,3,5,7,9]
```
```haskell
>> [1..] -- [1, 2, 3, ...]
-- try it, Ctrl+C for abort.
-- don't try with SublimeREPL!
```
```haskell
>> ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"
```
```haskell
>> [1.0,1.1..1.5] :: [Float]
[1.0,1.1,1.2,1.3000001,1.4000001,1.5000001]
```
```haskell
>> take 5 $ [0.1,0.2,..]
[0.1,0.2,0.30000000000000004,0.4,0.5]
```

---
### List comprehension
```Haskell
>> [x^2 | x <- [1..10]]
[1,4,9,16,25,36,49,64,81,100]
```
```Haskell
-- multiple generators
>> [(x, y) | x <- [1..3], y <- [1..x]]
(1,1),(2,1),(2,2),(3,1),(3,2),(3,3)]
```
```Haskell
-- assign lokas "variable"
>> [(x,k) | x <- [1..3], let k = x^2]
[(1,1),(2,4),(3,9)]
```
```Haskell
-- can use boolean guards
>> [x | x <- [1..10], sin x > 0.5]
[1.0,2.0,7.0,8.0]
```


---


### Working with lists

```haskell
head :: [a] -> a
>> head [1,2,3] -- 1
```
```haskell
tail :: [a] -> [a]
>> tail [1,2,3] -- [2,3]
```
```haskell
(++) :: [a] -> [a] -> [a]
>> [1,2,3] ++ [10, 20, 30] -- [1, 2, 3, 10, 20, 30]
```
```haskell
(:) :: a -> [a] -> [a]
>> 1 : [2, 3] -- [1, 2, 3]
```
```haskell
take :: Int -> [a] -> [a]
>> take 3 [1,2,3,4,5,6] -- [1, 2, 3]
```

---

### Working with lists
```haskell
>> length [1,2,3] -- 3
>> reverse [1,2,3] -- [3,2,1]
>> last [1,2,3] -- 3 (be careful with infinite lists)
>> init [1,2,3] -- [1,2] (here infinite is ok)
```
```haskell
map :: (a -> b) -> [a] -> [b]
>> map (^2) [1, 2, 3] -- [1, 4, 9]
```
```haskell
zip :: [a] -> [b] -> [(a, b)]
>> zip [1,2] ['a', 'b', 'c', 'd'] -- [(1,'a'),(2,'b')]
```
```haskell
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
>> zipWith (>) [13,22,36] [10, 48, 29] -- [True, False, True]
```



