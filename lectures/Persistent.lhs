---
title: A Persistent Set Interface
date: October 9, 2017
---

[persistentlhs]: Persistent.lhs
[persistent-sol]: Persistent-sol.html

*Note:* You may download the [lhs version][persistentlhs]
of this module. Eventually, the [complete version][persistent-sol] will
be made available.

> {-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}
> {-# OPTIONS_GHC -fdefer-type-errors #-}

> module Persistent where

> import Control.Monad
> import Test.QuickCheck hiding (elements)
> import Data.Maybe as Maybe
> import Data.List (sort,nub)

A persistent data structure is one where all of the operations are pure
functions.

For example, let\'s look at the interface of a simple *persistent* set.  We can
tell that the implementation is persistent just by looking at the *types* of
the operations. In particular, the `empty` operation is not a function, it is
just a set \-\- there is only one empty set. If we were allowed to mutate it,
it wouldn\'t be empty any more.

> class Set s where
>    empty    :: s a
>    member   :: Ord a => a -> s a -> Bool
>    insert   :: Ord a => a -> s a -> s a
>    elements :: Ord a => s a -> [a]


When we define an abstract data structure like `Set` above, using a type
class, we should also specify properties that *all* implementations should
satisfy.  What properties do you think a set should satisfy?

<FILL IN HERE>

Below, we will define these properties as QuickCheck tests.

Here\'s an example property: The empty set has no elements.

> prop_empty :: forall s. (Set s) => Bool
> prop_empty = null (elements (empty :: s Int))

Note that the type of this function is ambiguous \-\- the type variable `s`
only appears in the `Set` constraint. When we use this property, we need to
explicitly state what the type `s` should be because GHC cannot figure it out
on its own. We\'ll do this with a feature of GHC called \"TypeApplications\"
below.

The other tricky part of this definition is that, due to the ambiguity, we
need to add a type annotation to `empty`. Otherwise, GHC doesn\'t know what
empty set to use. However, we want our property to be generic, so we need a
different extension, called \"ScopedTypeVariables\" to bring the type variable
`s` into scope in the body of `prop_empty` using the keyword `forall`. That
way we can mention this type variable in the annotation `s Int`.

We can also define properties for set insertion.

> -- Anything inserted can be found in the set
> prop_insert :: forall s. (Set s) => Int -> s Int -> Bool
> prop_insert x y = member x (insert x y)

> -- We don't lose any set elements after an insertion.
> prop_insert_inequal :: forall s. (Set s) => Int -> Int -> s Int -> Property
> prop_insert_inequal x y s =
>     x /= y ==> member y s == member y (insert x s)

And for listing the elements of the set.

> -- No duplicates in the elements
> prop_elements :: forall s. (Set s) => s Int -> Bool
> prop_elements s = length (nub l) == length l where
>                         l = elements s

Define your own set property below (you\'ll probably need to adjust the type annotation):

> prop_myproperty :: forall s. (Set s) => Bool
> prop_myproperty = undefined

List implementation
-------------------

One trivial implementation of sets uses lists.

> instance Set [] where
>    empty    = []
>    member   = elem
>    insert   = (:)
>    elements = nub

Let\'s make sure our implementation satisfies properties of sets.

> main :: IO ()
> main = do
>   quickCheck $ prop_empty @[]
>   quickCheck $ prop_elements @[]
>   quickCheck $ prop_insert @[]
>   quickCheck $ prop_insert_inequal @[]
>   quickCheck $ prop_myproperty @[]

Note that in each case we provide the list type constructor `[]` as a type
argument to the property. (Type arguments must begin with `@` in Haskell. We
can only supply them if the `TypeApplications` extension is enabled.)

    *Persistent> :set -XTypeApplications
    *Persistent> quickCheck $ prop_empty @[]


Persistent vs\. Ephemeral
------------------------

* An *ephemeral* data structure is one for which only one version is
  available at a time: after an update operation, the structure as it
  existed before the update is lost.

  For example, conventional arrays are ephemeral.  After a location in an array
  is updated, its old contents are no longer available.

* A *persistent* structure is one where multiple version are
  simultaneously accessible: after an update, both old and new versions
  are available.

  For example, a binary tree can be implemented persistently, so that after
  insertion, the old value of the tree is still available.

Persistent data structures can sometimes be more expensive than their
ephemeral counterparts (in terms of constant factors and sometimes
also asymptotic complexity), but that cost is often insignificant
compared to their benefits:

   - better integration with concurrent programming (naturally lock-free)
   - simpler, more declarative implementations
   - better semantics for equality, hashing, etc.
   - access to *all* old versions (git for everything)

Next, we\'ll look at another persistent version of a common data structure:
Red-Black trees. These lectures demonstrate that functional programming is
adept at implementing sophisticated data structures. In particular, datatypes
and pattern matching make the implementation of persistent tree-like data
structures remarkably straightforward. These examples are drawn from Chris
Okasaki\'s excellent book [Purely Functional Data
Structures](http://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504).

However, we\'ll only scratch the surface. There are many
industrial-strength persistent data structures out there.

  * Finger trees/Ropes, see  [Data.Sequence](http://www.haskell.org/ghc/docs/7.6.3/html/libraries/containers-0.5.0.0/Data-Sequence.html)
  * Size balanced trees, see [Data.Map](http://www.haskell.org/ghc/docs/7.6.3/html/libraries/containers-0.5.0.0/Data-Map.html)
  * Big-endian Patricia trees, see [Data.IntMap](http://www.haskell.org/ghc/docs/7.6.3/html/libraries/containers-0.5.0.0/Data-IntMap.html)
  * Hash array mapped tries, used in the [Clojure](http://en.wikipedia.org/wiki/Hash_array_mapped_trie) language
  * and [many more](http://cstheory.stackexchange.com/questions/1539/whats-new-in-purely-functional-data-structures-since-okasaki)