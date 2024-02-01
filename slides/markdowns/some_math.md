# Some math

---

## Category

A category `\(\mathcal{C}\)` consists of:
* a class of objects `\(Ob(\mathcal{C})\)`
* a *set* `Hom($a$, $b$)` for each ` $a$, $b$ ` in `\(\mathop{Ob}(\mathcal{C})\)`
* a map

    `\(\circ\) : Hom($b$, $c$) \(\times\) Hom($a$, $b$) $\to$ Hom($a$, $c$)`

  for each ` $a$, $b$, $c$ ` in `\(Ob(\mathcal{C})\)`

---

Notation:

`$f:a \to b$ $\Leftrightarrow$ $f \in$ Hom($a$, $b$) `

`$g \circ f \Leftrightarrow \circ(g, f)$`

---

... such that:

* For all `$a$` exists `$id_a:a \to a $`:

`$$f \circ id_a = f, id_a \circ f' = f'$$`

* For any `$f : a \to b, g : b \to c, h : c \to d$`

`$$ (h\circ g)\circ f = h\circ (g\circ f)$$`

---

### Examples

* `Set` -- **all** sets + functions as morphism;
* `Monoid` -- monoids + homomorphisms;
* `Group` -- groups + homomorphisms;
* `Top` -- topological spaces + continues mappings;
* `POS` -- partially ordered sets + monotonic maps; 
* `Hask` -- haskell types + functions;
  * `$\circ$ = .`
---

## Functors
Functor `$F:\mathcal{C}\to \mathcal{D}$` = map between categories.

* for all `$a \in \mathcal{C}$` there is `$F(a) \in \mathcal{D}$`
* for `$f:a \to b$` there is `$F(f):F(a)\to F(b)$`

and

* `$F(id_a) = id_{F(a)}$ `
* `$F(g \circ f) = F(g) \circ F(f)$`

---

### Examples of functors

* `$\pi_n$ : Top $\to$ Group`
* `Forget: Monoid $\to$ Set`
* `Free: Set $\to$ Monoid`
  * `$F(A)$` is all finite lists with elements in `$A$`
  * `$F(f)$` is `map($f$)`

---

### Functor in Haskell

* 1-parametric type `t : * -> *` 
  
  = transformation `t : Hask $\to$ Hask`
    
  `t(a) = t a `

* 1-parametric type `Functor t => t` is a functor!
    
  ` f : a -> b  ==> fmap f : t a -> t b `

---

## Natural transformations

= map between functors `$F, G : \mathcal{C} \to \mathcal{D}$`!
* `$\phi : F \Rightarrow G$` is a collection of morphisms `$\phi_a : F(a) \to G(a)$` for all `$a \in \mathcal{C}$`...

---
... such that `$\forall f : a \to b $`
`$$ \phi_b \circ F(f) = G(f) \circ \phi_a$$`

<img alt="Diagram" src="pics/NaturalTransformation.svg" width="50%"/>
---

In terms of Haskell:
* `Functor t => t :: * -> *`
* `Functor t' => t' :: * -> *`
* `phi :: t a -> t' a`

such that 

`fmap f . phi = phi . fmap f`

---

### Examples

```Haskell
--- Maybe => []
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]
```
```Haskell
--- [] => Maybe
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x
```
```Haskell
--- Either e => Maybe
rightToMaybe :: Either e a -> Maybe a
rightToMaybe (Left _) = Nothing
rightToMaybe (Right x) = Just x
```
---

### Exercise 
Prove that all possible natural transformations 

`Id => []`

are of the form 

`phi n x = [x, ..., x]`,

`n = 0, 1, ..., $\infty$`

---

### Fun fact

* Functors `$F:\mathcal{C} \to \mathcal{D}$` with natural transformations form a category: `Func($\mathcal{C}$,$\mathcal{D}$)`.

---

## Monoidal category

Additional structure -- multiplication of objects.

* *Bifunctor* `$\otimes : \mathcal{C} \times \mathcal{C} \to \mathcal{C}$`.
* Unit object `$I \in \mathcal{C}$`.
* *Natural* isomorphisms:
  * `$$ (A \otimes B) \otimes C \cong A \otimes (B \otimes C) $$`
  * `$$ I \otimes A \cong A \cong A \otimes I $$`
---
### Examples

* `Set` with `$\otimes = \times$`, `$I = \{*\}$`
* `Hilb($\mathbb{R}$)`, with `$\otimes = \otimes$`, `$I = \mathbb{R}$`

* `Hask`, with ` $\otimes =$(,)`, `$I=$()`
  * `(a, (b, c)) $\cong$ ((a, b), c)`
  * `((), a) $\cong$ a $\cong$ (a, ())`
* `Func($\mathcal{C}$, $\mathcal{C}$)` with `$\otimes = \circ$`, `$I =$Id`


---

### Monoidal functors

* A functor, preserving monoidal structure

`$$ F(A) \otimes_\mathcal{D} F(B) \cong  F(A \otimes_\mathcal{C} B) $$`
`$$ I_\mathcal{D} \cong F(I_\mathcal{C})  $$`

---

### Lax monoidal functor

* Only translate monoidal structure

`$$ \phi_{AB} : F(A) \otimes_\mathcal{D} F(B) \to  F(A \otimes_\mathcal{C} B) $$`
`$$ \eta : I_\mathcal{D} \to F(I_\mathcal{C})  $$`
\+ coherence conditions
---
### In terms of Haskell

```Haskell
class Functor t => Monoidal t where
  phi :: (t a, t b) -> t (a, b)
  unit :: t ()
  -- must satisfy some "natural" conditions
  -- for example
  -- f :: a -> b, g :: c -> d  ==> 
  -- phi ( fmap f fa, fmap g gc ) = 
  --    fmap (\(x, y) -> (f x, g y)) (phi (fa, gc))
```

---

### Why do I need it?..
```Haskell
class Functor t => Monoidal t where
  phi :: (t a, t b) -> t (a, b)
  unit :: t ()
```
```Haskell
f :: a -> b
=> fmap f :: t a -> t b
```
Lax monoidal functors can "lift" functions with multiple parameters
```Haskell
f :: (a,b) -> c -- function with 2 arguments 

>> fmap f :: t (a, b) -> t c 
  -- not a function with 2 arguments
>> (fmap f . phi) :: (t a, t b) -> t c 
  -- function with 2 arguments! 
```

---
### Equivalent to Applicative!

```Haskell
class Applicative t => Monoidal t where
  phi :: (t a, t b) -> t (a, b)
  phi (ta, tb) = liftA2 (,) ta tb
  
  unit :: t ()
  unit = pure ()
```
```Haskell
class Monoidal t => Applicative t where
  liftA2 :: (a -> b -> c) -> t a -> t b -> t c
  liftA2 f ta tb = (fmap f . phi) (ta, tb)
  
  pure :: a -> t a
  pure a = fmap (const x) unit
```

---

## Monoid object
= object with operation and unit
* `$(\mathcal{C}, \otimes, I) $` -- monoidal category
* `$M$`-- object
* `$\eta : I \to M$` -- unit, 
* `$\mu : M \otimes M \to M$` -- multiplication

---

such that

<img alt="Monoid" src="pics/MonoidObject.svg" width="70%"/>

---

### Monad

... is a monoid object in `Fun($\mathcal{C}$, $\mathcal{C}$)`

---

### Monad

Explicitly: 
* Functor `$M : \mathcal{C} \to \mathcal{C}$`.
* Natural transformation `$\eta : Id \Rightarrow M$`.
* Natural transformation `$\mu : M \circ M \to M$`

such that...

---

### In Haskell

```Haskell
class Functor m => Monad m where 
  return :: a -> m a
  join :: m (m a) -> m a -- equivalent definition!
```
```Haskell
(>>=) ::  m a -> (a -> m b) -> m b
(>>=) ma f = join $ (fmap f) ma

join :: m (m a) -> m a
join x = x >>= id
```

---

## Kleisli category

* `$M : \mathcal{C} \to \mathcal{C}$` -- monad
* `$Ob(Kl(\mathcal{C})) = Ob(\mathcal{C})$`
* `$Hom_{Kl(\mathcal{C})}(a, b) = Hom_{\mathcal{C}}(a, M(b))$ `

given `$f : a \to M(b), g : b \to M(c)$`
`$$g\circ' f = \mu \circ M(g) \circ f $$`

---

### In Haskell

```Haskell
(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) f g = join . fmap g . f
```

**Three laws of monads**

(`>=>`) defines a Kleili category
* `return >=> f == f`
* `f >=> return == f`
* `(f >=> g) >=> h == f >=> (g >=> h)`
---

# Thanks a lot!
