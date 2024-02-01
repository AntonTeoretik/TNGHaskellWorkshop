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

### Lists
```haskell
>> [1..10]
[1,2,3,4,5,6,7,8,9,10]
```
```haskell
>> [1,3..11]
[1,3,5,7,9,11]
```
```haskell
>> [1..] -- [1, 2, 3, ...]
-- try it, Ctrl+C for abort.
-- don't try with SublimeREPL!
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



