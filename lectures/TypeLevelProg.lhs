---
title: Type Level Programming
date: Dec 05, 2018
---

[dephs]: ../code/Dep.hs

**Note** You may download the .hs file for this lecture [here][dephs] 

Haskell\'s type system has grown ever more expressive over the years, even including
type-level programming facilities and some ways to encode dependently typed programming.
Type level programming gives you interesting
new tools for designing software. You can guarantee safety properties, and in some cases,
even gain performance optimizations through the use of these types.

This lesson is a brief introduction to some type-level programming facilities provided
by GHC\'s language extensions. It also intendeds to give a taste of dependently
typed-programming in Haskell, through a simple standard example.

Let\'s start by reviewing basic data type definitions in Haskell.

Basic Types
===========

Consider the following Haskell datatype definitions:


~~~~~~~~ {.haskell}
data Unit = MkUnit

data Bool = True | False
~~~~~~~~~~~~~~~~~~

This code block defines two new types: `Unit` and `Bool`. The `Unit` type has one constructor,
called `MkUnit`. Since there\'s only one constructor for this type, and it takes no parameters,
there is only one value of this type. We call it `Unit` because there\'s only one value.

`Bool` is a type that has two constructors: `True` and `False`. These don\'t take any
parameters either, so they\'re kind of like constants.

What does it mean to be a type? A type is a way of classifying things. Things \-\- that\'s
a vague word. What do I mean by \"things\"? Well, for these simple types above, we\'ve already
seen all their possible values \-\- we can say that `True` and `False` are members of the
type `Bool`. Furthermore, `1`, `'a'`, and `Unit` are not members of the type `Bool`.

Let’s look at another type:

~~~~~~~~ {.haskell}
data IntAndChar = MkIntAndChar Int Char
~~~~~~~~~~

This introduces a type `IntAndChar` with a single constructor that takes two arguments:
one of which is an `Int` and the other is a `Char`. Values of this type look like:

~~~~~~~~ {.haskell}
theFirstOne = MkIntAndChar 3 'a'
theSecond   = MkIntAndChar (-3) 'b'
~~~~~~~~~~~

`MkIntAndChar` looks a lot like a function. In fact, if we ask GHCi about it\'s type,
we get this back:

~~~~~~~~ {.haskell}
ghci> :t MkIntAndChar
MkIntAndChar :: Int -> Char -> IntAndChar
~~~~~~~~~~~

`MkIntAndChar` is a function accepting an `Int` and a `Char` and finally yeilding a
value of type `IntAndChar`.

So we can construct values, and values have types. Can we construct types?
And if so, what values do they have?


Higher Kinds
=============

Let\'s hold onto our intuition about functions and values. A function with the type
`foo :: Int -> IntAndChar` is saying:

     Give me a value with the type Int, and I will give you a value with the type IntAndChar.

Now, let\'s lift that intuition into the type level. A value constructor accepts a
value and yields a value. So a type constructor accepts a type and yields a type.
Haskell\'s type variables allow us to express this.

Let\'s consider everyone\'s favorite sum type:

~~~~~~~~ {.haskell}
data Maybe a
    = Just a
    | Nothing
~~~~~~~~~~~

Here, we declare a type `Maybe`, with two data constructors: `Just`, which accepts a
value of type `a`, and `Nothing`, which does not accept any values at all. Let\'s ask
GHCi about the types of `Just` and `Nothing`!

~~~~~~~~ {.haskell}
ghci> :t Just
Just :: a -> Maybe a

ghci> :t Nothing
Nothing :: Maybe a
~~~~~~~~~~~~~

So `Just` has a function type \-\- and it looks like, whatever type of value we give it,
it becomes a `Maybe` of that type. `Nothing`, however, can conjure up whatever type it
wants, without needing a value at all. Let\'s play with that a bit:

~~~~~~~~ {.haskell}
ghci> let nothingA = Nothing :: Maybe a
ghci> let nothingInt = Nothing :: Maybe Int
ghci> let nothingChar = Nothing :: Maybe Char
ghci> nothingInt == nothingChar

\<interactive\>:27:15: error:
    Couldn't match type ‘Char’ with ‘Int’
    Expected type: Maybe Int
      Actual type: Maybe Char
    In the second argument of ‘(==)’, namely ‘nothingChar’
    In the expression: nothingInt == nothingChar
    In an equation for ‘it’: it = nothingInt == nothingChar

ghci> nothingA == nothingInt
True
ghci> nothingA == nothingChar
True
~~~~~~~~~~~~~~

Woah \-\- we get a type error when trying to compare `nothingInt` with `nothingChar`,
since `(==)` only works on values that have the same type. But then,
wait, why does `nothingA` not complain when compared with `nothingInt` and `nothingChar`?

The reason is that `nothingA :: Maybe a` really means:

      I am a value of Maybe a, for any and all types a that you might provide to me.

So, we see that we\'re passing types to `Maybe`, in much the same way that we pass
values to `MkIntAndChar`. Let\'s ask GHCi about the type of `Maybe`!

~~~~~~~~ {.haskell}
ghci> :type Maybe

\<interactive\>:1:1: error:
    Data constructor not in scope: Maybe
    Perhaps you meant variable ‘maybe’ (imported from Prelude)
~~~~~~~~~~~~~~

Well, it turns out that types don\'t have types (kind of, sort of).
Types have **kinds**. The relation between kinds and types is the same as between types
and values; that is, \"kind\" means \"type of type\". We can ask GHCi about the kind
of types with the `:kind command`:

~~~~~~~~ {.haskell}
ghci> :kind Maybe
Maybe :: * -> *
~~~~~~~~~~~~~~~

What is `*` doing there? Well, `*` is the kind of types which have values. So `Maybe`
has the kind `* -> *`, which means:

     Give me a type that has values, and I will give you a type that has values.

Accordingly, Haskell\'s kind system accepts `Maybe Int`, but rejects `Maybe Maybe`
and `Bool Int` as ill-kinded.

Maybe you\'ve heard about higher kinded polymorphism before. Let\'s write a data type
that demonstrates what this means:

~~~~~~~~ {.haskell}
data HigherKinded f a
    = Bare a
    | Wrapped (f a)
~~~~~~~~~~~~~~

Haskell\'s kind inference is awfully kind on the fingers \-\- since we use `f` applied
to `a`, Haskell just knows that `f` must have the kind `* -> *`. If we ask GHCi about
the kind of `HigherKinded`, we get back:

~~~~~~~~ {.haskell}
ghci> :kind HigherKinded
HigherKinded :: (* -> *) -> * -> *
~~~~~~~~~~~~~~~~

So `HigherKinded` is a type that accepts a type of kind `* -> *`, and a type of kind `*`,
and returns a type of kind `*`. In plain, verbose English, this reads as:

     Give me two types: the first of which is a function that does not have values itself,
     but when given a type that does have values, it can have values. The second being a
     type that has values. Finally, I will return to you a type that can have ordinary values.

Remember that `*` is the kind of any type that has values, or even types that are only
inhabited by the infinite loop:

~~~~~~~~~~{.haskell}
ghci> data Void
ghci> :kind Void
Void :: *
~~~~~~~~~~~~~~~~

We don\'t provide any ways to construct a void value, yet it still has kind `*`.

Let\'s now encode our first type level numbers. We\'ll start with the Peano natural
numbers, where numbers are inductively defined as either Zero or the Successor of some
natural number.

~~~~~~~~~{.haskell}
data Zero
data Succ a

type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ (Succ (Succ (Succ Zero)))
~~~~~~~~~~~~

But this is pretty unsatisfying. After all, there\'s nothing that stops us from saying
`Succ Bool` or `Maybe Zero`, which doesn\'t make any sense. We already know about the
benefits of types for clarifying thinking and preventing errors, so abandoning the safety
of types when programmig our types just seems silly. In order to get that safety back,
we need to introduce more kinds than merely `*`. For this, we have to level up our GHC.


Promoted datatypes and DataKinds
=================================

~~~~~~~{.haskell}
{-# LANGUAGE DataKinds #-}
~~~~~~~~~~~~~~~~~

GHC\'s `DataKinds` language extension allows us to promote data constructors into
type constructors, which also promotes their type constructors into kind constructors.


Now, let\'s define our kind safe type level numbers:

~~~~~~~{.haskell}
data Nat = Zero | Suc Nat
~~~~~~~~~~~~~~~

In plain Haskell, this definition introduces a new type `Nat` with two value constructors,
`Zero` and `Succ` (which takes a value of type `Nat`). With the `DataKinds` extension,
this also defines some extra new tidbits. We get a new kind `Nat`, which exists in a
separate namespace. And we get two new types: a type constant `'Zero`, which has the
kind `Nat`, and a type constructor `'Succ`, which accepts a type of kind `Nat`.
Let\'s ask GHCi about our new buddies:

~~~~~~~{.haskell}
ghci> :kind 'Zero
'Zero :: Nat
ghci> :kind 'Suc
'Succ :: Nat -> Nat
~~~~~~~~~~~~~

You might think: that looks familiar! And it should. After all, the types look very
much the same!

~~~~~~~{.haskell}
ghci> :type Zero
Zero :: Nat
ghci> :type Suc
Succ :: Nat -> Nat
~~~~~~~~~~~~~~~~~

  **Some notes on name resolving**
Haskell separates name spaces for types and values, so we can use same identifier
for types and values at the same time. But this habits leads to ambiguity together
with `DataKinds` extension. For example, what does `()` at type-level context mean?
There are two possible interpretations:

 * Unit type of the kind `*`, and
 * Promoted type of the promoted kind `()`.
 
In such case, we prefix `'` to the type name for the second case and otherwise its
interpreted as the first case. That is, in type context, `'()` is the promoted type
from the data constructor `()` and has kind `()`, and `()` is the unit type of the
kind `*`.

It\'s important to note that there are no values of type `'Zero`.
The only kind that can have types that can have values (or inhabitant) is `*`.
So the type introduced by DataKinds (e.g. `'Zero` or `'Suc ('Suc 'Zero)` as type)
cannot have any inhabitant. Specifically, in a function type signature, such a types
can only be the argument of other types, but cannot occur by itself alone
(what is the inhabitant of type `Suc n`?).

We\'ve gained the ability to construct some pretty basic types and kinds.
In order to actually use them, though, we need a bit more power.

GADTs
=======

~~~~~~~~~ {.haskell}
{-# LANGUAGE GADTs #-}
~~~~~~~~~~~~~~~~~

You may be wondering, \"What does GADT stand for?\" Richard Eisenberg will tell you
that they\'re *Generalized Algebraic Data Types*, but that the terminology isn\'t
helpful, so just think of them as GADTs.

GADTs are a tool we can use to provide extra type information by matching on constructors.
They use a slightly different syntax than normal Haskell data types.
Let\'s check out some simpler types that we\'ll write with this syntax:

~~~~~~~ {.haskell}
data Maybe a where
    Just :: a -> Maybe a
    Nothing :: Maybe a
~~~~~~~~~~~~
    
The GADT syntax lists the constructors line-by-line, and instead of providing the
fields of the constructor, we provide the type signature of the constructor.
This is an interesting change \-\- I just wrote out `a -> Maybe a`, which suggests
that I can make these whatever type I want.

~~~~~~~~ {.haskell}
data IntBool a where
    Int :: Int -> IntBool Int
    Bool :: Bool -> IntBool Bool
~~~~~~~~~~~~~~

This declaration creates a new type `IntBool`, which has the kind `* -> *`.
It has two constructors: `Int`, which has the type `Int -> IntBool Int`, and `Bool`,
which has the type `Bool -> IntBool Bool`.

Since the constructors carry information about the resulting type, we get bonus
information about the type when we pattern match on the constructors!
Check this signature out:

~~~~~~~~ {.haskell}
extractIntBool :: IntBool a -> a
extractIntBool (Int _)  = 0
extractIntBool (Bool b) = b
~~~~~~~~~~~~

Something really interesting is happening here! When we match on `Int`,
we know that `IntBool a ~ IntBool Int`. That `~` tilde is a symbol for type equality,
and introduces a constraint that GHC needs to solve to type check the code.
For this branch, we know that `a ~ Int`, so we can return an `Int` value.

We now have enough power in our toolbox to implement everyone\'s favorite example
of dependent types: length indexed vectors!

Vectors - Avoiding Boundary Erros Using the Type-System
=======================================================

Length indexed vectors allow us to put the length of a list into the type system,
which allows us to statically forbid out-of-bounds errors. We have a way to promote
numbers into the type level using `DataKinds`, and we have a way to provide bonus
type information using GADTs. Let\s combine these two powers for this task.

Here is a definition of a length-indexed vector:

~~~~~~~~{.haskell}
data Vec :: * -> Nat -> * where
   Nil :: Vec a 'Zero
   (:>) :: a -> Vec a n -> Vec a ('Succ n)
infixr 5 :>
~~~~~~~~~~~~~~~

The `Vec` type is parameterized by both the type of the vector elements and the length
of the vector. A value constructed by `Nil` can have any type `a`, but the length is
always constrained to be `'Zero`. The `(:>)` (analogous to list `(:)`) constructor takes
two values: one of type `a`, and another of type `Vec a n`. We don\'t know how long the
Vector provided is \–\- it can be any `n` such that `n` is a natural number (of type `Nat`).
We do know that the length of resulting vector is the successor of that number, though.

Thus `True :> Nil` has type `Vec Bool 1` and `'x' :> 'y' :> 'z' :> Nil`
has type `Vec Char 3`.

Datatype promotion allows the datatype `Nat` to be used in the kind of `Vec`, and
correspondingly the `'Zero` and `'Suc` promoted data constructors appear in the types
of `Nil` and `(:>)` . Moreover, `Vec a m` is a generalised algebraic datatype or GADT,
meaning that pattern matching on its constructors supplies information to the typechecker:
a proof of the equation `m ∼ Zero` in the `Nil` branch, and a proof of `m ∼ Suc n` in
the `(:>)` branch.

This type-level knowledge of length is useful for expressing more precise invariants
in types, leading to more reliable code. The `head` and `tail` functions for vectors
are definesd as:

~~~~~~~~ {.haskell}
head :: Vec a (Suc n) -> a
head (x :> _) = x

tail :: Vec a (Suc n) -> Vec a n
tail (x :> xs) = xs
~~~~~~~~~~~~~~~

This definitions statically enforce the invariant that the argument list must be
non-empty, so this definition is total. Moreover, it is guaranteed that `tail`
returns a result of the right length.

Having defined the `Vec` data type, we can derive standard type-class instances.
We need to use GHC\'s `StandaloneDeriving` language extension, instead of
deriving clause:

~~~~~~~~ {.haskell}
deriving instance Eq a => Eq (Vec a n)
deriving instance Show a => Show (Vec a n)
~~~~~~~~~~~~~~

Let\'s now write an operation that appends two vectors. We already need to think carefully
about types, because the types include information about the vectors\' lengths.
In this case, if we combine a `Vec a n` and a `Vec a m`, we had surely better get
a `Vec a (n+m)`. Because we are working over our `Nat` type, we must first define
addition on type-level data. We will need to use **Type Families** to provide the
definition of operations on type-level data.

Type Families
=============

For some reason, functions that operate on types are called *type families*.
We will need GHC\'s language extension:

~~~~~~~ {.haskell}
{-# LANGUAGE TypeFamilies #-}
~~~~~~~~~~~~~~~

There are two ways to write a type family: open, where anyone can add new cases,
and closed, where all the cases are defined at once. Let\'s see some examples. 

~~~~~ {.haskell}
type family F1 a where
   F1 Int  = Bool
   F1 Char = Double

useF1 :: F1 Int -> F1 Char
useF1 True =1.0
useF1 False = (−1.0)
~~~~~~~~~~~~~~~~~~

We see that GHC simplifies `F1 Int` to `Bool` and `F1 Char` to `Double` in order to
type-check `useF1`.

`F1` is a closed type family, in that all of its defining equations are given in one place.
This most closely corresponds to what functional programmers expect from their functions.
Today\'s Haskell also supports open type families, where the set of defining equations
can be extended arbitrarily. Open type families interact particularly well with Haskell\'s
type classes, which can also be extended arbitrarily. Here is a more interesting example
than the one above:

~~~~~~~ {.haskell}
type family Element c
class Collection c where
   singleton :: Element c -> c

type instance Element [a] = a
instance Collection [a] where
   singleton x = [x]

type instance Element (Set a) = a
instance Collection (Set a) where
   singleton = Set.singleton
~~~~~~~~~~~

Because the type family `Element` is open, it can be extended whenever a programmer
creates a new collection type. Often, open type families are extended in close
correspondence with a type class, as we see here. For this reason, GHC supports
associated open type families, using this syntax:

~~~~~~~ {.haskell}
class Collection’ c where
   type Element’ c
   singleton’ :: Element’ c -> c

instance Collection’ [a] where
   type Element’ [a] = a
   singleton’ x = [x]

instance Collection’ (Set a) where
   type Element’ (Set a) = a
   singleton’ = Set.singleton
~~~~~~~~~~~~

Associated type families are essentially syntactic sugar for regular open type families.


Type-level Natural Numbers Addition
------------------------------------

We can now use type familes for defining the type-level addition for natural numbers and
then use it in the type of vector concatenation (With GHC\'s `TypeOperators` language
extension, \"natural number addition\" can writen as infix operator):

~~~~~~~~~ {.haskell}
{-# LANGUAGE TypeFamilies, TypeOperators, UdecidableInstances #-}

infixl 6 :+

type family (m :: Nat) :+ (n :: Nat) :: Nat
type instance 'Zero  :+ n = n
type instance 'Suc m :+ n = 'Suc (m :+ n)
~~~~~~~~~~~

Let\'s check our definition:

~~~~~~~~~ {.haskell}
ghci> :kind (Succ (Succ Zero)) :> (Succ Zero)
(Succ (Succ Zero)) :> (Succ Zero) :: Nat

ghci> :kind! (Succ (Succ Zero)) :> (Succ Zero)
(Succ (Succ Zero)) :> (Succ Zero) :: Nat
= 'Succ ('Succ ('Succ 'Zero))
~~~~~~~~~~~~~~~~

The first line just tells us that the result of adding two natural numbers is
itself a natural number. The second line shows the actual result of evaluating
the type level function. Cool!

Vectors appending
==================

So now we can finally finish writing `append`.

~~~~~~~~{.haskell}
append :: Vec a n -> Vector a m -> Vec a (n :+ m)
~~~~~~~~~~~~~~~

Let\'s start with some bad attempts, to see what the types buy us:

~~~~~~~~{.haskell}
append Nil ys = Nil
~~~~~~~~~~

This fails with a type error \-\- cool!

~~~~~~~~{.haskell}
    Could not deduce: m ~ 'Zero
    from the context: n ~ 'Zero
      bound by a pattern with constructor:
                 VNil :: forall a. Vector 'Zero a,
               in an equation for ‘append’
      at /home/matt/Projects/dep-types.hs:31:8-11
    ‘m’ is a rigid type variable bound by
      the type signature for:
        append :: forall (n :: Nat) a (m :: Nat).
                  Vector n a -> Vector m a -> Vector (Add n m) a
      at /home/matt/Projects/dep-types.hs:30:11
    Expected type: Vector (Add n m) a
      Actual type: Vector 'Zero a
    In the expression: VNil
    In an equation for ‘append’: append VNil rest = VNil
    Relevant bindings include
      rest :: Vector m a
        (bound at /home/matt/Projects/dep-types.hs:31:13)
      append :: Vector n a -> Vector m a -> Vector (Add n m) a
        (bound at /home/matt/Projects/dep-types.hs:31:1)
~~~~~~~~~~~~

The error is kinda big and scary at first. Let\'s dig into it a bit.

GHC is telling us that it can\'t infer that `m` (which is the length of the second
parameter vector) is equal to `Zero`. It knows that `n` (the length of the first parameter)
is `Zero` because we\'ve pattern matched on `Nil`. So, what values can we return?

We need to construct a value `Vec a m`, and we have been given a value `Vec a m`.
BUT \-\- we don\'t know what `m` is! So we have no way to spoof this or fake it.
We have to return our input. So our first case is simply:

~~~~~~~~{.haskell}
append Nil ys = ys
~~~~~~~~

Like with addition of natural numbers, we\'ll need to have the inductive case.
Since we have the base case on our first parameter, we\'ll want to try shrinking
our first parameter in the recursive call.

So let\'s try another bad implementation:

~~~~~~~~{.haskell}
append (a :> xs) ys = append xs (a :> ys)
~~~~~~~~~~

This doesn\'t really do what we want, which we can easily verify:

~~~~~~~~ {.haskell}
ghci> append (1 :> (3 :> Nil)) (2 :> Nil)
3 :> (1 :> (2 :> Nil))
~~~~~~~~~~~~~

The answer should be `1 :> (3 :> (2 :> Nil))`. However, our Vector type only encodes
the length of the vector in the type. The sequence is not considered.
Anything that isn\'t lifted into the type system doesn\'t get any correctness guarantees.

So let\'s fix the implementation:

~~~~~~~~~ {.haskell}
append :: Vec a m -> Vec a n -> Vec a (m :+ n)
append Nil       ys = ys
append (x :> xs) ys =  x :> (append xs ys)
~~~~~~~~~~~~~


Replicate
==========

Now let\'s try to write a vector version of the `replicate` function,
which creates a vector of length `n` by repeating its second argument `n` times:

~~~~~~~{.haskell}
replicate :: Nat -> a -> Vector a n
replicate Zero    _ = Nil
replicate (Suc n) a = a :> replicate n a
~~~~~~~~~~~~~

This code seems good, but won\'t compile! To see why, let\'s focus on its type signature:
`replicate :: Nat -> a -> Vector a n`. 
Here, first argument with type `Nat` is intended to be the length of the resulting vector.
But, the parameter `n` of resulting type can be any natural number and is independent of
the first argument! This is why the above code won\'t type-check.

Then, how about the following signature?

~~~~~~~{.haskell}
replicate :: n -> a -> Vector a n
~~~~~~~~~~~~~

Hmm. Let\'s write the function body:

~~~~~~~{.haskell}
replicate :: n -> a -> Vector a n
replicate -- ?
~~~~~~~~~~~~~

Wait! `n` has kind `Nat` and hence does not have any inhabitant,
so we cannot pattern match on the first argument!

So the problems we have to solve is as follows:

  1. We have to pass the type-level natural as function argument, and

  2. pattern matching on type-level natural so that we can write recursive function.
  
Since types of the kind `Nat` can ocurr only as a parameter of other type, we have
to define some data-type carrying `Nat` type as its parameter and the structure of
its data constructors should reflect the one of corresponding type-level natural.
With GADTs, we can define such a data-type:

~~~~~~~{.haskell}
data SNat n where
  SZ :: SNat Zero
  SS :: SNat n -> SNat (Suc n)
~~~~~~~~~~~~~~~

Here, for each type-level natural `n`, there is exactly one term with the type `SNat n`
and its structure is isomorphic to `n`. For example, `SNat Zero` has `SZ` as its only
inhabitant,  `SNat (Suc (Suc Z))` has `SS (SS SZ)`, and so on.

Such a data-type is called the *singleton* for promoted types, and was introduced by
[Richard Eisenberg and Stephanie Weirich][eisenberg]. Singletons can be defined for
any promoted data-types. For example, we can define singleton type `SBool` for
promoted type of the kind `Bool` as follows:

~~~~~~~{.haskell}
data SBool b where
  STrue  :: SBool True
  SFalse :: SBool False
~~~~~~~~~~~~~~~

And, we can define an operation between the singleton types to treat the type-level
arithmetic. For example, singleton function for natural addition `+` can be
implemented as follows:

~~~~~~~{.haskell}
infixl 6 :+

(%:+) :: SNat n -> SNat m -> SNat (n :+ m)
SZ   %:+ m = m
SS n %:+ m = SS (n :+ m)
~~~~~~~~~~~~~~~

Function `replicate`can now be implemented as:

~~~~~~~{.haskell}
replicate :: SNat n -> a -> Vec a n
replicate SZ     _ = Nil
replicate (SS n) a = a :> replicate n a
~~~~~~~~~~~~~~~

It is too boring to define such singletons by hand for all promoted types and
type-level functions. Fortunately, Eisenberg\'s singletons package provides the
functionality to automatically do that \-\- but this is beyond the scope of this
introductory lecture. 

The Length of a Vector
=======================

Let\'s now try to implement a function that returns the length of a Vector.
At first, this might seem trivial \-\- the length is right there in the type!
However, we must be careful here. While the length is indeed in the type,
types are erased in Haskell and thus that length is not automatically available
at runtime for computation. Thus, we would have to compute the length as: 

~~~~~~~ {.haskell}
length :: Vec a n -> SNat n
length Nil = SZ
length (_ :> v ) = SS (length v)
~~~~~~~~~~~~~~~~~~

As you see, dependently-typed programming in Haskell is not always natural and easy.
More recently, some Haskell extensions have been proposed in order to allow more flexible
and natural dependently-typed programing in Haskell, for example: *Dependent Haskell*,
proposed by [Eisenberg][eisenberg2], and the *inch* language, proposed by [Gundry][gundry].



Futher Reading 
================

 * [Eisenberg\'s PhD Thesis][eisenberg2] defines *Dependent Haskell*, which supports
 full dependent types via a backward-compatible extension to today\'s Haskell. Chapter 2
 of this thesis provides a nice brief review of current Haskell extensions which allow
 type-level programming and several dependently-typed programming examples are discussed
 in Chapter 3.
 
 * [Gundry thesis][gundry] proposes an langyage \-\- *inch \-\- that extends Haskell with
 type-level data and functions, and dependent product types and presents applications of
 *inch* based on type-level arithmetic.

[eisenberg]: https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf
[eisenberg2]: https://arxiv.org/abs/1610.07978
[gundry]: https://pdfs.semanticscholar.org/4a05/d76d1c582d0e1cab117542424941f9d43372.pdf?_ga=2.30034556.2125290800.1544542305-1724655041.1544542305