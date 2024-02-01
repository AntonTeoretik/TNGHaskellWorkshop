# Custom types

---

### General idea

* Type = set.
* Define type = define **data constructors**.

---

### Enum (sum type)
```Haskell
data Color = Red | Green | Blue

getColorCode :: Color -> String
getColorCode Red = "#ff0000"
getColorCode Green = "#00ff00"
getColorCode Blue = "#0000ff"
```
```Haskell
>> getColorCode Red
"#ff0000"
>> :t Red
Red :: Color 
```
* `Red`, `Green`, `Blue` -- data constructors.
* `Color` ≅ `*` ⊔ `*` ⊔ `*`



---

### Product type
```
data Point2D = Pt2D Double Double

dist :: Point2D -> Point2D -> Double
dist (Pt2D x y) (Pt2D x' y') = sqrt $ (x - x')**2 + (y - y')**2
```
```Haskell
>> dist (Pt2D 0 0) (Pt2D 1 1)
1.4142135623730951
>> :t Pt2D
Pt2D :: Double -> Double -> Point2D 
```

* `Pt2D` -- data constructor.
* `Point2D` ≅ `Double` × `Double`. 
* *Warning*: all elements of `Point2D` are of the form `Pt2D x y`.
* Ok to rename `Pt2D` to `Point2D`

---

### Sum of products

```Haskell
data Shape = Circle Double            
           | Rectangle Double Double
```

* `Shape` ≅ `Double` ⊔ `Double`  × `Double`
* `Circle :: Double -> Shape`
* `Rectangle :: Double -> Double -> Shape`

---

### General pattern

```Haskell
data TYPE_NAME = CON_1 TYPE1_1 ... TYPE1_m1            
               | CON_2 TYPE2_1 ... TYPE1_m2
               ...
               | CON_n TYPEn_1 ... TYPEn_mn
```

* Some of `TYPEi_j` may be equal to `TYPE_NAME`...

---

### Recursive types

```Haskell 
data MyList = Nil 
            | Elem Double MyList

lenghtMyList :: MyList -> Integer
lenghtMyList Nil = 0
lenghtMyList (Elem _ ls) = 1 + lenghtMyList ls
```
```Haskell
>> lenghtMyList (Elem 2 $ Elem 4 $ Elem 8 Nil)
3
```

---

### Infix data constructor

```Haskell 
infixr 5 :+
data MyList = Nil
            | (:+) Double MyList -- or "Double :+ MyList"
  
lenghtMyList :: MyList -> Integer
lenghtMyList Nil = 0
lenghtMyList (_ :+ ls) = 1 + lenghtMyList ls
```
```Haskell
>> lenghtMyList $ 1 :+ 2 :+ 3 :+ Nil
3
```

* Must start with `:` . 
* For usual lists `:` IS the infix data constructor.

---

### Record syntax (named fields)

```Haskell
data Person = Person {
    firstsName :: String,
    lastName :: String,
    age :: Int }
  deriving (Show, Eq)
```
```Haskell  
>> let john = Person "John" "Smith" 21
>> firstName john
"John"
```
```Haskell
>> let john = Person {age = 33, lastName = "Smith", 
                           firstName = "John"} 
-- can change the order
```
---

### Record syntax (named fields)

```Haskell
data Person = Person {
    firstsName :: String,
    lastName :: String,
    age :: Int }
  deriving (Show, Eq)
```
```Haskell
welcomeBill :: Person -> String
welcomeBill ( Person {firstName = "Bill"} ) = "Welcome, Bill"
welcomeBill _ = "You are not Bill".

>> let bill = Person "Bill" "Ivanov" 33
>> welcomeBill bill
Person {firstName = "John", lastName = "Ivanov", age = 33}
```
```Haskell
changeName :: String -> Person -> Person
changeName new_name pers = pers {firstName = new_name} 

>> let bill = Person "Bill" "Ivanov" 17
>> changeName "John" bill
-- this will create NEW person, bill is still Bill.
```

---

### Parametric types

```Haskell
data Point2D a = -- 1 type parameter
    Point2D a a
  deriving (Show, Eq)

data MyPair a b = -- 2 type parameters
    MyPair a b
  deriving (Show, Eq)
```
```Haskell
>> :t Point2D
Point2D :: a -> a -> Point2D a -- "Point2D a" is a full type name 
>> :k Point2D
Point2D :: * -> * 
``` 
```Haskell
>> :k MyPair
MyPair :: * -> * -> *
```

---

### Type constructor

```Haskell
data MyPair a b = MP a b

```
* `MyPair` is a *type constructor*
* `:kind MyPair` is `* -> * -> *`
* "Function" (transformation), which takes 2 type parameters and "returns" type

```Haskell 
>> :k MyPair String Int
MyPair String Int :: *
>> :k MyPair String
MyPair String :: * -> * -- 1-parameric type!
```
---

### Maybe
```Haskell
data Maybe a = Nothing | Just a    
```
```Haskell
findOdd :: [Integer] -> Maybe Integer
findOdd [] = Nothing
findOdd (x : xs) | x `mod` 2 == 0  = Just x
                 | otherwise       = findOdd xs
```
* "Same" as `Optional<T>` in Java
* `Maybe Integer` is a type
* `Maybe` is a **type constructor**
---

### Either
```Haskell
data Either a b = Left a | Right b
```
```Haskell
findOdd :: Int -> [Integer] -> Either String Integer
findOdd _ [] = Left "There is no odd numbers in the list"
findOdd n (x : xs) | n < 0           = Left "Maximum depth reached"
                   | x `mod` 2 == 0  = Right x
                   | otherwise       = findOddSafe (n-1) xs
```
```Haskell
>> findOdd 100 [1,3..]
Left "Maximum depth reached"

>> findOdd 100 [1,3..50]
Left "There is no odd numbers in the list"

>> findOdd 100 $ [1,2,3]
Right 2
```

---

### `->`
*  `->` is a type constructor!

```Haskell
>> :k (->)
(->) :: * -> * -> *
```
```Haskell
>> :i (->)
data (->) (a :: TYPE q) (b :: TYPE r) -- old version
```

