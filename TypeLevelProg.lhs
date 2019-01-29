---
title: Type Level Programming
date: Dec 05, 2018
---

[classes]: TypeClasses.html 

> {-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE UndecidableInstances, FunctionalDependencies #-}
> {-# LANGUAGE PolyKinds, ConstraintKinds, RankNTypes #-}
> {-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE
    AllowAmbiguousTypes,
    DataKinds,
    FlexibleContexts,
    FlexibleInstances,
    GADTs,
    InstanceSigs,
    MultiParamTypeClasses,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}
    
Haskell\'s type system has grown ever more expressive over the years, even including
type-level programming facilities. This lesson is a brief introduction to these facilities. 

Type Classes and Dictionaries
==============================

As we have seen in a [previous lesson][classes], Haskell supports type classes. Let\'s
review this concept through an example:

> class Show a where
>    show :: a → String
>
> instance Show Bool where
>    show True = "True"
>     show False = "False"

This declares the class `Show`, parameterized over a type variable `a`,
with one method `show`. The class is then instantiated at the type `Bool`, with
a custom implementation of `show` for `Bool`s. Note that, in the `Show Bool` instance,
the `show` function can use the fact that `a` is now `Bool`: the one argument to
`show` can be pattern-matched against `True` and `False`. This is in stark contrast
to the usual parametric polymorphism of a function `show’ :: a → String`, where the
body of `show’` cannot assume any particular instantiation for `a`.

With `Show` declared, we can now use this as a constraint on types. For example:

> smooshList :: Show a ⇒ [a] → String
> smooshList xs = concat (map show xs) 

The type of `smooshList` says that it can be called at any type `a`, as long as there
exists an instance `Show a`. The body of `smooshList` can then make use of the `Show a`
constraint by calling the `show` method. If we leave out the `Show a` constraint, then
the call to `show` does not type-check. This is a direct result of the fact that the
full type of `show` is really `Show a ⇒ a -> String`. (The `Show a` constraint on `show`
is implicit, as the method is declared within the `Show` class declaration.) Thus, we
need to know that the instance `Show a` exists before calling `show` at type `a`.

Operationally, type classes work by passing [dictionaries][dict]. A type class dictionary
is simply a record containing all of the methods defined in the type class. It is as if
we had these definitions:

~~~~~~~{.haskell}
     data ShowDict a = MkShowDict {showMethod :: a → String }

     showBool :: Bool -> String
     showBool True = "True"
     showBool False = "False"

     showDictBool :: ShowDict Bool
     showDictBool = MkShowDict showBool
~~~~~~~~~~~~~~~~~~~~~~~~

Then, whenever a constraint `Show Bool` must be satisfied, GHC produces the dictionary
for `showDictBool`. This dictionary actually becomes a runtime argument to functions
with a `Show` constraint. Thus, in a running program, the `smooshList` function actually
takes two arguments: the dictionary corresponding to `Show a` and the list `[a]`.


Families
==========

Type Families
--------------

A [type family][typefamily] is simply a function on types. Here is an uninteresting example
(GHC\'s `TypeFamilies` language extension must be used in order to allow type famiiy
declarations):

> type family F1 a where
>    F1 Int  = Bool
>    F1 Char = Double
>
> useF1 :: F1 Int -> F1 Char
> useF1 True =1.0
> useF1 False = (−1.0)

We see that GHC simplifies `F1 Int` to `Bool` and `F1 Char` to `Double` in order to
type-check `useF1`.

F1 is a closed type family, in that all of its defining equations are given in one place.
This most closely corresponds to what functional programmers expect from their functions.
Today\'s Haskell also supports open type families, where the set of defining equations
can be extended arbitrarily. Open type families interact particularly well with Haskell\'s
type classes, which can also be extended arbitrarily. Here is a more interesting example
than the one above:

> type family Element c
> class Collection c where
>    singleton :: Element c -> c
>
> type instance Element [a] = a
> instance Collection [a] where
>    singleton x = [x]
>
> type instance Element (Set a) = a
> instance Collection (Set a) where
>    singleton = Set.singleton

Because the type family `Element` is open, it can be extended whenever a programmer
creates a new collection type. Often, open type families are extended in close
correspondence with a type class, as we see here. For this reason, GHC supports
associated open type families, using this syntax:

> class Collection’ c where
>    type Element’ c
>    singleton’ :: Element’ c -> c
>
> instance Collection’ [a] where
>    type Element’ [a] = a
>    singleton’ x = [x]
>
> instance Collection’ (Set a) where
>    type Element’ (Set a) = a
>    singleton’ = Set.singleton

Associated type families are essentially syntactic sugar for regular open type families.

**Partiality in type families:** A type family may optionally be partial, in that it is
not defined over all possible inputs. This poses no problems in the theory or practice
of type families. If a type family is used at a type for which it is not defined, the
type family application is considered to be stuck. For example:

> type family F2 a
> type instance F2 Int = Bool

Suppose there are no further instances of `F2`. Then, the type `F2 Char` is stuck.
It does not evaluate, and is equal only to itself. It is impossible for a Haskell
program to detect whether or not a type is stuck, as doing so would require
pattern-matching on a type family application \—\- this is not possible. This is a
good design because a stuck open type family might become unstuck with the inclusion
of more modules, defining more type family instances. Stuckness is therefore fragile
and may depend on what modules are in scope.

Data families
--------------

A data family defines a family of datatypes. An example shows best how this works:

> data family Array a -- compact storage of elements of type a
> data instance Array Bool = MkArrayBool ByteArray
> data instance Array Int = MkArrayInt (Vector Int)

With such a definition, we can have a different runtime representation for `Array Bool`
than we do for `Array Int`, something not possible with more traditional parameterized types.

Rich kinds
===========

Kinds in Haskell98
-------------------

With type families, we can write type-level programs. But are our type-level programs
correct? We can gain confidence in the correctness of the type-level programs by ensuring
that they are *well-kinded*. Indeed, GHC does this already. For example, if we try to say
`Element Maybe`, we get a type error saying that the argument to `Element` should have kind `⋆`,
but `Maybe` has kind `⋆ -> ⋆`.

Kinds in Haskell are not a new invention; they are precisely defined in the Haskell98 report.
Because type constructors in Haskell may appear without their arguments, Haskell needs a
kinding system to keep all the types in line. For example, consider the library definition
of Maybe:

~~~~~~~~~{.haskell}
    data Maybe a = Nothing | Just a
~~~~~~~~~~

The word `Maybe`, all by itself, does not really represent a type. `Maybe Int` and `Maybe Bool`
are types, but `Maybe` is not. The type-level constant `Maybe` needs to be given a type to
become a type. The kind-level constant `⋆` contains proper types, like `Int` and `Bool`.
Thus, Maybe has kind `⋆ -> ⋆`. Accordingly, Haskell\'s kind system accepts `Maybe Int`
and `Element [Bool]`, but rejects `Maybe Maybe` and `Bool Int` as ill-kinded.

Promoted datatypes
-------------------

The kind system in Haskell98 is rather limited. It is generated by the grammar

~~~~~~~~~{.haskell}
    k ::= * | * -> *
~~~~~~~~~~~~~~~

and that\'s it. When we start writing interesting type-level programs, this
almost-unityped limitation bites. For example, previous to recent innovations,
Haskellers wishing to work with natural numbers in types would use these declarations:

~~~~~~~{.haskell}
   data Zero
   data Succ a
~~~~~~~~~~~~~~~

We can now discuss `Succ (Succ Zero)` in a type and treat it as the number 2.
However, we could also write nonsense such as `Succ Bool` and `Maybe Zero`.
These errors do not imperil type safety, but it is natural for a programmer who
values strong typing to also pine for strong kinding.

Accordingly, [Yorgey et al.][yorgey] introduce *promoted datatypes* (allowed
by GHC\'s `DataKinds` language extension). The central idea
behind promoted datatypes is that when we say

~~~~~~~{.haskell}
data Bool = False | True
~~~~~~~~~~~~~~~

we declare two entities: a type `Bool` inhabited by terms `False` and `True`; and a
kind `Bool` inhabited by types `'False` and `'True`. We can then use the promoted
datatypes for more richly kinded type-level programming.

A nice, simple example is type-level addition over promoted unary natural numbers:

> data Nat = Zero | Succ Nat
> type family a + b where
> 'Zero   + b = b
> 'Succ a + b = 'Succ (a+b)

Now, we can say `'Succ 'Zero + 'Succ ('Succ 'Zero)` and GHC will simplify the type to
`'Succ ('Succ ('Succ 'Zero))`. We can also see here that GHC does kind inference on
the definition for the type-level `+`. We could also specify the kinds ourselves like this:

> type family (a :: Nat) + (b :: Nat) :: Nat where ...


** Some notes on name resolving **

Haskell separates name spaces for types and values, so we can use same identifier
for types and values at the same time. But this habits leads to ambiguity together
with `DataKinds` extension. For example, what does () at type-level context mean?
There are two possible interpretations:

 * Unit type of the kind `*`, and
 * Promoted type of the promoted kind `()`.
 
In such case, we prefix `'` to the type name for the second case and otherwise its
interpreted as the first case. That is, in type context, `'()` is the promoted type
from the data constructor `()` and has kind `()`, and `()` is the unit type of the
kind `*`.

Another example is the list. Then `'[]` stands for the promoted empty list of kind
`[k]`, and `[]` for the type constructor of the kind `* -> *`.
The same convension applies to the alphabetical names.


Kind polymorphism
------------------

Another contribution of the work of Yorgey et al. is to enable kind polymorphism
(allowed by GHC\'s `PolyKinds` language extesion).
Kind polymorphism is nothing more than allowing kind variables to be held abstract,
just like functional programmers frequently do with type variables.
For example, here is a type function that calculates the length of a type-level
list at any kind:

> type family Length (list :: [k]) :: Nat where
> Length '[ ]     = 'Zero
> Length (x ':xs) = 'Succ (Length xs)

Kind polymorphism extends naturally to constructs other than type functions.
Consider this datatype:

> dataT f a=MkT (f a)

With the PolyKinds extension enabled, GHC will infer a most-general kind
∀ k. (k -> ⋆) -> k -> ⋆ for `T`. In Haskell98, on the other hand, this type
would have kind `(⋆ -> ⋆) -> ⋆ -> ⋆`, which is less general.

A kind-polymorphic type has extra, *invisible* parameters that correspond to kind
arguments. Invisible means that the arguments do not appear in Haskell source code.
With the -fprint-explicit-kinds flag, GHC will print kind parameters when they occur.
Thus, if a Haskell program contains the type `T Maybe Bool` and GHC needs to print
this type with -fprint-explicit-kinds, it will print `T * Maybe Bool`, making the `*`
kind parameter visible.

Constraint kinds
-----------------

[Bolingbroke][bolingbroke] introduced constraint kinds to GHC (allowed by GHC\'s
`ConstraintKinds` language extension). Haskell allows constraints
to be given on types. For example, the type `Show a => a -> String` classifies a function
that takes one argument, of type `a`. The `Show a` constraint means that `a` is required
to be a member of the `Show` type class. Constraint kinds make constraints fully first-class.
We can now write the kind of `Show` as `⋆ -> Constraint`. That is, `Show Int` (for example)
is of kind `Constraint`. `Constraint` is a first-class kind, and can be quantified over.
A useful construct over Constraints is the `Some` type:

> data Some :: (* -> Constraint) -> * where
> Some :: c a ⇒ a -> Some c

If we have a value of `Some Show`, stored inside it must be a term of some
(existentially quantified) type `a` such that `Show a`. When we pattern-match against the
constructor `Some`, we can use this `Show a` constraint. Accordingly, the following function
type- checks (where `show :: Show a => a -> String` is a standard library function):

> showSomething :: Some Show -> String
> showSomething (Some thing) = show thing

Note that there is no `Show a` constraint in the function signature \-\- we get the
constraint from pattern-matching on `Some`, instead.

The type `Some` is useful if, say, we want a heterogeneous list such that every element
of the list satisfies some constraint. That is, each element of `[ Some Show ]` can be
a different type `a`, as long as `Show a` holds:

> heteroList :: [Some Show ]
> heteroList = [Some True, Some (5 :: Int), Some (Just ())]
>
> printList :: [ Some Show ] -> String
> printList things = "[" + intercalate ", " (map showSomething things) + "]"

~~~~~~~{.haskell}
     ghci> putStrLn $ printList heteroList
     [True,5,Just ()]

~~~~~~~~~~~~~

Generalized algebraic datatypes
================================

Generalized algebraic datatypes (or GADTs) are a powerful feature that allows term-level
pattern matches to refine information about types (uses GHC\'s `GADTs` language extension).

Here, we introduce one particularly important GADT: propositional equality. The following
definition appears now as part of the standard library shipped with GHC, in the
`Data.Type.Equality` module:


> data (a::k) :∼: (b::k) where
>   Refl ::a:∼:a

The idea here is that a value of type `t1 :∼: t2` (for some `t1` and `t2`) represents
evidence that the type `t1` is in fact equal to the type `t2`. Here is a use of this type,
also from `Data.Type.Equality`:

> castWith :: (a :∼: b) -> a -> b
> castWith Refl x = x

Here, the `castWith` function takes a term of type `a :∼: b` \-\- evidence that
`a` equals `b` \-\- and a term of type `a`. It can immediately return this term, `x`,
because GHC knows that `a` and `b` are the same type. Thus, `x` also has type `b` and
the function is well typed.

Note that `castWith` must pattern-match against `Refl`. The reason this is necessary
becomes more apparent if we look at an alternate, entirely equivalent way of defining
`(:∼:)`:

~~~~~~~~{.haskell}
    data(a::k) :~: (b::k) where
       Refl :: (a ~ b) => a :~: b
~~~~~~~~~~~

In this variant, we define the type using the Haskell98-style syntax for datatypes.
This says that the `Refl` constructor takes no arguments, but does require the constraint
that `a ~ b` . The constraint `( ~ )` is GHC\'s notation for a proper type equality constraint.
Accordingly, to use `Refl` at a type `t1 :∼: t2`, GHC must know that `t1 ∼ t2` \-\-
in other words, that `t1` and `t2` are the same type. When `Refl` is matched against,
this constraint `t1 ~ t2` becomes available for use in the body of the pattern match.

Returning to `castWith`, pattern-matching against `Refl` brings `a ∼ b` into the context,
and GHC can apply this equality in the right-hand side of the equation to say that `x` has
type `b`.

Operationally, the pattern-match against `Refl` is also important. This match is what forces
the equality evidence to be reduced to a value. As Haskell is a lazy language, it is possible
to pass around equality evidence that is `undefined`. Matching evaluates the argument, making
sure that the evidence is real.

Higher-rank types
=================

Haskell98 uses, essentially, the [Hindley-Milner (HM)][hindley-milner] type system.
The HM type system allows only prenex quantification, where a type can quantify over
type variables only at the very top. The system is based on types, which have no
quantification, and type schemes, which do:

      t ::= a | C | t1 -> t2   -- types
      σ ::= ∀α.σ | t           -- type schemes


Here,  `α` stands for any of a countably infinite set of type variables and `C` to stand
for any type constant (including (->)).

Let-bound definitions in HM are assigned type schemes; lambda-bound definitions are assigned
monomorphic types, only. Thus, in HM, it is appropriate to have a function
`length  :: ∀a.[a] -> Int` but disallowed to have one like `bad :: (∀a. a -> a -> a) -> Int`:
`bad`\'s type has a `∀` somewhere other than at the top of the type. This type is of
the *second rank*, and is forbidden in HM.

On the other hand, today\'s GHC allows types of arbitrary rank
(through GHC\'s `RankNTypes` languageextension). A full example
of the usefulness of this ability would take us too far afield (see, for example,
[Lämmel and Peyton Jones][lamel] and [Washburn and Weirich][wirich] for implementations
that make critical use of this ability). The cost, however, is that higher-rank types
cannot be inferred. For this reason, this definition of `higherRank`

~~~~~~~~{.haskell}
    higherRank f = (f True, f ’x’)
~~~~~~~~~~~~~~~~~~

will not compile without a type signature. Without the signature, GHC tries to unify
the types `Char` and `Bool`, failing. However, providing a signature

~~~~~~~~{.haskell}
     higherRank :: (∀ a. a → a) → (Bool , Char )
~~~~~~~~~~~~~

does the trick nicely.


Functional dependencies
========================

[Functional dependencies][fundep] are GHC\'s earliest feature introduced to enable rich
type-level programming (uses GHC\'s `FunctionalDependencies` language extension).
They are, in many ways, a competitor to type families.
With functional dependencies, we can declare that the choice of one parameter to a type
class fixes the choice of another parameter. For example:

> class Pred (a::Nat) (b::Nat) | a->b
> instance Pred 'Zero 'Zero
> instance Pred ( 'Succ n) n

In the declaration for class `Pred` (\"predecessor\”), we say that the first parameter, `a`,
determines the second one, `b`. In other words, `b` has a functional dependency on `a`.
The two instance declarations respect the functional dependency, because there are no two
instances where the same choice for `a` but differing choices for `b` are made.

Functional dependencies are, in some ways, more powerful than type families.
For example, consider this definition of Plus:

> class Plus (a :: Nat ) (b :: Nat ) (r :: Nat ) | a b -> r , r a -> b
> instance Plus 'Zero bb
> instance Plus a b r ⇒ Plus ('Succ a) b ('Succ r)

The functional dependencies for `Plus` are more expressive than what we can do for
type families. (However, see the work of [Stolarek et al.][stolarek], which attempts to
close this gap.) They say that `a` and `b` determine `r`, just like the arguments to
a type family determine the result, but also that `r` and `a` determine `b`.
Using this second declared functional dependency, if we know `Plus a b r` and `Plus a b' r`,
we can conclude `b = b'`. Although the functional dependency `r b -> a` also holds,
GHC is unable to prove this and thus we cannot declare it.

Functional dependencies have enjoyed a rich history of aiding type-level programming.
Yet, they require a different paradigm to much of functional programming. When writing
term-level definitions, functional programmers think in terms of functions that take
a set of arguments and produce a result. Functional dependencies, however, encode
type-level programming through relations, not proper functions.


Dependent Types
================

Dependent types allow term-level data into the static type system. This allows
more precise invariants to be specified: for example, rather than the type of lists
of arbitrary length, one can work with the type of vectors of a statically-known
length. Let\'s start with the standard example of Vector to understand how to write
dependently-typed programsin Haskell.

Vectors - Avoiding Boudary Erros Using the Type-System
-------------------------------------------------------

Here is a definition of a length-indexed vector:

> data Nat = Zero | Succ Nat  -- first, some natural numbers
> data Vec :: * -> Nat -> * where
>    Nil :: Vec a 'Zero
>    (:>) :: a -> Vec a n -> Vec a ( 'Succ n)
> infixr 5 :>


The `Vec` type is parameterized by both the type of the vector elements and the length of
the vector. Thus `True :> Nil` has type `Vec Bool 1` and `'x' :> 'y' :> 'z' :> Nil`
has type `Vec Char 3`.

Datatype promotion allows the datatype `Nat` to be used in the kind of `Vec`, and
correspondingly the `Zero` and `Suc` data constructors appear in the types of `Nil`
and `(:>)` . Moreover, `Vec a m` is a generalised algebraic datatype or GADT,
meaning that pattern matching on its constructors supplies information to the typechecker:
a proof of the equation `m ∼ Zero` in the `Nil` branch, and a proof of `m ∼ Suc n` in
the `(:>)` branch.

This type-level knowledge of length is useful for expressing more precise invariants
in types, leading to more reliable code. The `head` and `tail` functions for vectors
are definesd as:

> head :: Vec a (Suc n) -> a
> head (x :> _) = x
>
> tail :: Vec a (Suc n) -> Vec a n
> tail (x :> xs) = xs

This definitions statically enforce the invariant that the argument list must be
non-empty, so this definition is total. Moreover, it is guaranteed that `tail` returnes
a result of the right length.

Having defined the `Vec` data type, we can derive standard type-class instances.
We need to use GHC\'s `StandaloneDeriving` language extension, instead of deriving clause:

> deriving instance Eq a => Eq (Vec a n)
> deriving instance Show a => Show (Vec a n)


** append **

Let\'s write an operation that appends two vectors. We already need to think carefully
about types, because the types include information about the vectors\' lengths.
In this case, if we combine a `Vec a n` and a `Vec a m`, we had surely better get
a `Vec a (n+m)`. Because we are working over our `Nat` type, we must first define
addition on type-level data. 

As we have seen, type families allow the definition of operations on type-level data.
Addition for type-level naturals can be defined, then used in the type of vector
concatenation (With GHC\'s `TypeOperators` language extension, "natural number addition"
can writen as infix operator):

> infixl 6 :+
>
> type family (m :: Nat) :+ (n :: Nat) :: Nat
> type instance Zero  :+ n = n
> type instance Suc m :+ n = Suc (m :+ n)

With the above definition, Vector\'s appending can be written as:  

> append :: Vec a m -> Vec a n -> Vec a (m :+ n)
> append Nil       ys = ys
> append (x :> xs) ys =  x :> (append xs ys)

**Note** Type families do not correspond exactly to term-level functions,
because they are open, that is, defining equations can be added anywhere.
They are not translated into case analysis, but are understood as rewrite rules
on the syntax of type expressions. This gap between the term-level and type-level
operational semantics is problematic for dependent types, where the same expression
may be used both statically (in the typechecker) and dynamically (at runtime).

** replicate ** 

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

Hmm. Let\s write the function body:

~~~~~~~{.haskell}
replicate :: n -> a -> Vector a n
replicate -- ?
~~~~~~~~~~~~~

Wait! `n` has kind `Nat` and hence does not have any inhabitant,
so we cannot pattern match on the first argument!

So the problems we have to solve is as follows:

  # We have to pass the type-level natural as function argument, and
  # pattern matching on type-level natural so that we can write recursive function.
  
For types of the kind `Nat` can be ocurred only as a parameter of other type, we have
to define some data-type carrying `Nat` type as its parameter and the structure of
its data constructors should reflect the one of corresponding type-level natural.
With GADTs, we can define such a data-type:

~~~~~~~{.haskell}
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)
~~~~~~~~~~~~~~~

Here, for each type-level natural `n`, there is exactly one term with the type `SNat n`
and its structure is isomorphic to `n`. For example, `SNat Z` has `SZ` as its only
inhabitant,  `SNat (S (S Z))` has `SS (SS SZ)`, and so on.

Such a data-type is called the singleton for promoted types, and was introduced by
Richard [Eisenberg and Stephanie Weirich][eisenberg]. Singletons can be defined for
any promoted data-types. For example, we can define singleton type `SBool` for
promoted type of the kind `Bool` as follows:

~~~~~~~{.haskell}
data SBool b where
  STrue  :: SBool True
  SFalse :: SBool False
~~~~~~~~~~~~~~~

And, we can define the operation between the singlton types to treat the type-level
arithmetic. For example, singleton function for natural addition `+` can be
implemented as follows:

~~~~~~~{.haskell}
infixl 6 :+

(%:+) :: SNat n -> SNat m -> SNat (n :+ m)
SZ   %:+ m = m
SS n %:+ m = SS (n :+ m)
~~~~~~~~~~~~~~~





replicate :: Π (n :: N) → a → Vec a n replicate Zero = Nil
replicate (Suc n) x = Cons x (replicate n x)

Here the result type Vec a n depends on n, but the operational behaviour of the function also makes uses of n, as it is defined by pattern matching. This shows the need for the dependent product Π: it is a function space where the value is available both statically and dynamically. GHC Haskell does not currently support Π, but it can be encoded in some cases. Adding Π to Haskell is the main contribution of part II of this thesis. Chapter 5 describes the resulting language.











**Notes** 


[weirich]:
[fundeps]:
[lamel]: 
[hindley-milner]: 
[bolingbroke]:
[yorgey]: 
[typefamily]: 
[dict]: 
