
# Expressions and functions

---
### General idea
* A program on Haskell is a set of expressions
* To execute a program in Haskell is to reduce ("evaluate") an expression

```Haskell
>> 3 + 4 * 5
23
-- 3 + 4 * 5 ~> 3 + 20 ~> 23

>> max (2 + 5) 10 + 12
20
-- max (2 + 5) 10 + 12 ~> max 7 10 + 12 ~> 10 + 12 ~> 20
```

---

### Arithmetic
`a + b`, `a - b`, `a * b`, `a / b`, `a ** b`, `a ^ b`, `(-a)`

`a == b`, `a /= b`, `a > b`, `a < b`, `a <= b`, `a >= b`

```Haskell
>> 3 ^ 2
9

>> 4 > 2
True

>> 3 /= 3
False
```
---

### Boolean
`a && b`, `a || b`, `not a`

`True`, `False`
```Haskell
>> True && False
False

>> (2 == 3) || True
True

>> not False
True
```
---

### if then else
`if (boolean expr) else (expr 1) then (expr 2)`

* `expr1` and `expr2` must be of the same *type*

```Haskell
>> if (1 > 0) then 5 else 10
5

>> if ( False ) then 42 else (-5)
-5

{-
    if 2 * 2 == 4 then 42 else 100 ~>
    if 4 == 4 then 42 else 100 ~>
    if True then 42 else 100 ~> 
    42 
-}

```
---

### Error and undefined

* `error "Message"` -- terminates the program with the given message.
* `undefined` -- terminates the program with the standard message.
* Lazy execution! 
```Haskell
>> if (1 > 0) then 4 else (error "My error message")
4
```
---

### Let ... in

* Used when you need to denote some large expression (or several).

`let Name1 = Expr1; Name2 = Expr2; ... in Expression`

```Haskell
>> let x = max 5 6; y = 20^2 - 399 in x + y
7


{-
    let x = max 5 6; y = 20^2 - 399 in x + y ~>
    let x = 6; y = 400 - 399 in x + y ~>
    let x = 6; y = 1 in x + y ~>
    let x = 6; y = 1 in 6 + 1 ~>
    6 + 1 ~> 7
-}
```

---

### Functions

* Functions = named expressions 
* `f x y z` stands for `f(x, y, z)`
* All functions are pure (same arguments -> always same result)
  * except some magic (IO) 
* function with 0 parameters = constants

```Haskell
-- floor a, sqrt a, max a b, min a b, ...
>> floor 3.5
3

>> max 2 3
3

>> pi
3.141592653589793
```

---

### Define custom function

`NAME PARAM1 PARAM2 ... = EXPRESSION`

* `NAME` must start with a small letter
* Can use `_` and `'`

```Haskell
add3 a b c = a + b + c
summOfSquares a b = a^2 + b^2
safeDiv a b = if b /= 0 then a / b else (error "Division by zero!")

>> add3 1 2 3
6

```
---

### Recursion

One can use recursion

```Haskell
factorial n = if n <= 0 then 1 else n * factorial (n - 1)
```

or cross-references

```Haskell
foo n = if n <= 0 then 0 else 2 * bar (n - 1)

bar n = if n <= 0 then 0 else 3 * foo (n - 1)

```

---

### Pattern matching

```Haskell
factorial' 0 = 1   
factorial' n = n * factorial' (n - 1) 

-- factorial' 3 ~> 3 * factorial' (3 - 1) ~> 
-- 3 * factorial' 2 ~> 3 * (2 * factorial' 1) ~>
-- 3 * ( 2 * (1 * factorial' 0) ) ~> 3 * (2 * (1 * 1)) ~> 6
```

* tries to match from top to bottom
  * infinite recursion:
  
```Haskell
factorial' n = n * factorial' (n - 1)
factorial' 0 = 1   
```

* use '`_`' when do not care about the value

```Haskell
-- const -- const is a standart function so, add'.
const' a _ = a
```

---
### Case of

* Pattern matching inside functions

```Haskell
complexExpression n = 
  if n > 1 
  then (
    if n `mod` 2 == 0 
    then (complexExpression $ n `div` 2) 
    else (complexExpression $ 3 * n + 1)
  ) else 1   

f n = case complexExpression (n + 2) of
    1 -> "Nice, nice, nice"
    _ -> "Something went wrong"
```
---

### Guards

```Haskell
factorial'' 0 = 1
factorial'' n | n > 0 = n * factorial'' (n-1)
              | otherwise = error "Argument must be positive"
```
* Any boolean expression can be a guard
* Check from top to bottom
* Not necessarily mutually exclusive
* `otherwise == True`

---

### Where
```Haskell
function a b c = average a b c + max3 a b c + factorial''' a
  where
    average x y z = (x + y + z) / 3
    max3 x y z = max x (max y z)
    
    factorial''' 0 = 1
    factorial''' n | n > 0 = n * factorial''' (n-1)
                   | otherwise = error "Invalid argument"
```

* `where` is a syntax sugar, NOT AN EXPRESSION
* Inside you can define anything, even other functions
* Nested `where`'s are ok, but can avoid them
```Haskell
zero = foo x y
  where
    x = 2 ^ 10
    foo x y = x - y - one
    one = 1
    y = 1023
```

---

### Operators = function
```Haskell
>> 2 + 3
5

>> (+) 2 3
5

>> (*) 3 4 
12

>> (||) False True
True
```

```Haskell
>> let sumOfSq x y = x^2 + y^2
>> 2 `sumOfSq` 5
29
```
---

### Priority

Each operator has a priority -- a number from `0` to `9`

```Haskell
8: ^, **
7: *, / 
6: +, -
4: ==, /=, <, <=, =>, >
3: &&
2: ||
```

---

### Associativity
* Left-associative: `+`, `-`, `*`, `/`
* Right-associative: `^`, `**`, `&&`, `||`
* Non-associative: `==`, `/=`, `<=`, `>=`, `<`, `>`

```Haskell
3 ^ 2 ^ 2 == 3 ^ (2 ^ 2)

16 / 4 / 2 == (16 / 4) / 2

-- 3 == 3 == 3 -- error! 
```

---

### Custom operators

* Can create custom operators!
* `!`, `#`, `$`, `%`, `&`, `*`, `+`, `.`, `/`, `<`, `=`, `>`, `?`, `@`, `\ `, `^`, `|`, `-`, `~`, `:`
    * don't start with `:`

```Haskell
infixl 8 *^* -- left associative
(*^*) x y = x^2 + y^2
-- x *^* y = x^2 + y^2 also correct

infix 8 =/^-.-^\= -- non associative
(=/^-.-^\=) str _ = str ++ ", cat!"
```
```Haskell
>> 2 *^* 4 *^* 10
500
-- 2 *^* 4 *^* 10 ~> (2*2 + 4*4) + 10*10 ~> 500
>> "Hello, " =/^-.-^\= "World!"
"Hello, cat!"
```

---

### Lambda functions

```Haskell
sum3 a b c = a + b + c
sum3' a b = \c -> a + b + c
sum3'' a = \b -> \c -> a + b + c
sum3''' = \a -> \b -> \c -> a + b + c
sum3'''' = \a b c -> a + b + c
```

* `\` because it looks like `λ`
* Can be used in any expression

```Haskell
>> 2 + (\x -> x + 2) 2
6
```