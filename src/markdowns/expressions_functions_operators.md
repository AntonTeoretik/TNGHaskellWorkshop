
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
```Haskell

 a + b,  a - b,  a * b,  a / b,  a ** b,  a ^ b,  (-a) 

 a == b,  a /= b,  a > b,  a < b,  a <= b,  a >= b 

 a && b,  a || b,  not a 

True, False
```
---

### Functions

* Functions = named expressions 
* `f x y z` stands for `f(x, y, z)`
*  Pure (same arguments -> always same result)
    * except some magic (`IO`) 
* function with 0 parameters = constant

---
### Operators = functions

* `f x y` <=> ```x `f` y```
* `x ++ y` <=> `(++) x y` 

---

### if then else
```Haskell
if (boolean expr) then (expr 1) else (expr 2)
```

* `expr1` and `expr2` must be of the same *type*

---

### Error and undefined

* `error "Message"` -- terminates the program with the given message.
* `undefined` -- terminates the program with the standard message.
* Lazy execution! 
---

### Let ... in

* Used when you need to denote some large expression (or several).

  ```Haskell
    let Name1 = Expr1
        Name2 = Expr2
        ...
    in Expression
  ```
* Can be inlined with `;`

  ```Haskell
  let Name1 = Expr1; Name2 = Expr2; ... in Expression
  ```

---

### Custom functions

```
NAME PARAM1 PARAM2 ... = EXPRESSION
```
* `NAME` must start with a small letter
* Allowed to use `_` and `'` in `NAME`
* Recursion / cross-references are allowed 

---

### Pattern matching

```Haskell
name p11 p12 ... p1n = expr_1
name p21 p22 ... p2n = expr_2
...
name pm1 pm2 ... pmn = expr_m

```

* `pij` are
  * literals
  * variables
  * wildcard `_`
  * structural patterns*
* tries to match from top to bottom

---
### Case of

* Pattern matching inside functions
* It is an expression (can be used anywhere where expression expected)

```Haskell
case expression of
  p1 -> expr_1
  p2 -> expr_2
  ...
  pn -> expr_n
```
---

### Guards

```Haskell
function p1 p2 ... pn 
  | boolean_1(p1, ..., pn) = expr_1
  | boolean_2(p1, ..., pn) = expr_2
  ...
  | boolean_m(p1, ..., pn) = expr_m
```
* Any boolean expression can be a guard
* Check from top to bottom
* Not necessarily mutually exclusive
* `,` = `&&`
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
* Nested `where`'s are ok, but you can avoid them

---

### Working with tuples
* `(a, b, c, ..., z)`
* Return tuple:

  ```
  f p1 ... pm = (e1, e2, ..., en)
  ```

* Pattern matching if tuple is a parameter

  ```Haskell
  f (0, b) = 10
  f (a, b) = a + b
  ```

* `fst`, `snd` for 2-tuples

---

### Working with lists

* List is either `[]` or `x : xs`

```Haskell
[a1, a2, a3, ..., an] == a1 : a2 : a3 : ... : an : []
```

* Pattern matching:

```Haskell
function [] = expr1
function (x : xs) = expr2
```

---


### Custom operators

* `!`, `#`, `$`, `%`, `&`, `*`, `+`, `.`, `/`, `<`, `=`, `>`, `?`, `@`, `\ `, `^`, `|`, `-`, `~`, `:`
    * don't start with `:`

```Haskell
infixl 8 *^* -- left associative, priority 8
(*^*) x y = x^2 + y^2
-- x *^* y = x^2 + y^2 also correct

infix 8 =/^-.-^\= -- non associative, priority 8
(=/^-.-^\=) str _ = str ++ ", cat!"
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
