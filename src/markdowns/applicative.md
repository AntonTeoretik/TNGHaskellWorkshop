# Applicative

---

### Problem

* `f :: a -> b -> c`
* `fmap f :: f a -> f(b -> c)`
* Want a way to lift function to 
  
`f a -> f b -> f c`

---

### Applicative

```Haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b -- infixl 4
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
```
```Haskell
(<*>) = liftA2 id
liftA2 f ax ay = (fmap f) ax <*> ay 
 -- = f <$> ax <*> ay
```

---

### `Maybe` is Applicative
```Haskell
pure = Just
```
```Haskell
>> liftA2 (+) (Just 2) (Just 3)
Just 5
>> pure (+) <*> Just 2 <*> Just 3
Just 5
```
```Haskell
>> pure (+) <*> Nothing <*> Just 3
Nothing
>> Nothing <*> Just 2 <*> Just 3
Nothing 
```
---
### `Either e` is Applicative
```Haskell
>> pure (/) <*> Right 10.0 <*> Right 2.0
>> Right 5.0
```
```Haskell
>> pure (/) <*> Left "Some error" <*> Left "Other error"
>> Left "Some error"
```


---
### `[]` is Applicative
```Haskell
pure x = [x]
```
```Haskell
>> pure (+) <*> [1, 2] <*> [10, 20]
[11, 21, 12, 22]
```
```Haskell
>> pure (+) <*> [1, 2] <*> [10, 20]
[11, 21, 12, 22]
```
```Haskell
>> [(+), (*)] <*> [1, 2] <*> [10, 20]
[11,21,12,22,10,20,20,40]
```

---
### `(,) a` is Applicative
but only if `a` is Monoid
```Haskell
instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u <> v, f x)
  liftA2 f (u, x) (v, y) = (u <> v, f x y)
```
```
>> ("(+) is applied to ", (+)) <*> (" 2 ", 2) <*> ("and 3", 3)
("(+) is applied to  2 and 3",5)
```

---
### `(->) e` is Applicative
```Haskell
instance Applicative ((->) r) where
    pure = const
    
    (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b) 
    (<*>) f g = \x -> (f x) (g x)
    
    liftA2 :: (a -> b -> c) -> (r -> a) -> (r -> b) -> (r -> c)
    liftA2 q f g = \x -> q (f x) (g x)
```
```Haskell
data Person = Person {
    name :: String,
    surname :: String
  } deriving (Show, Eq)

p :: Person
p = Person "Harry " "Potter"
```
```Haskell
>> pure (++) <*> name <*> surname $ p
"Harry Potter"
```

---

### Applicative laws

```Haskell
-- Identity
 pure id <*> v = v
```
```Haskell
-- Composition
 pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```
```Haskell
-- Homomorphism
 pure f <*> pure x = pure (f x)
```
```Haskell
-- Interchange
 u <*> pure y = pure ($ y) <*> u
```




