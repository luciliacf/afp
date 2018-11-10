---
title: Monoids and Foldables
date: September 25, 2017
---

[category]: https://bartoszmilewski.com/2011/01/09/monads-for-the-curious-programmer-part-1/
[currying]: https://wiki.haskell.org/Currying

> module MonoidsFoldables where
>
> import Data.Monoid
> import Data.Foldable as F

Semigroups and Monoids
-----------------------

Haskell is a great language for constructing code modularly from small but orthogonal building blocks.
One of these small blocks is the *monoid*. Although monoids come from mathematics (algebra in particular)
they are found everywhere in computing. You probably use one or two monoids implicitly with every line of
code you write, whatever the language, but you might not know it yet. By making them explicit we find
interesting new ways of constructing those lines of code. In particular, ways that are often easier to
both read and write. So the following is an intro to monoids in Haskell.

A monoid is a set with an associative binary operation `<>` for *combining* two elements of that set to make
another element of the same set, together with an element \-\- `mempty` \-\- that when it\'s combined with other
elements it leaves the other element unchanged. In other words, monoids obey the following equational rules:

     mempty <> x   = x                 -- left identity
     x <> mempty   = x                 -- right identity
     (x <> y) <> z = x <> (y <> z)     -- associativity

A great example is lists. Given two lists, say `[1,2]` and `[3,4]`, you can join them together using
`++` to get `[1,2,3,4]`. There\'s also the empty list `[]`. Using `++` to combine `[]` with any list
gives you back the same list, for example `[]++[1,2,3,4]==[1,2,3,4]`.

In Haskell, the `Monoid` typeclass is a subclass of typeclass `Semigroup`. The `Semigroup` typeclass
represents a set with an associative bynary operation `<>`. Semigroups have no other restrictions and
are a very general typeclass.


~~~~~{.haskell}
class Semigroup m where
  (<>) :: m -> m -> m
 
  infixr 6 <>
~~~~~~~~~~

The `Monoid` typeclass defines three methods: 

~~~~~{.haskell}
class Semigroup m => Monoid m where
  mempty :: m
 
  -- defining mappend is unnecessary, it copies from Semigroup
  mappend :: m -> m -> m
  mappend = (<>)
 
  -- defining mconcat is optional, since it has the following default:
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
~~~~~~~~~~

Note that `mconcat = foldr mappend mempty` is a simpler, specialised, `foldr`, in which we do not need to
specify the combining function nor initial accumulator, as we simply use `mappend` (i.e. `(<>)`) and `mempty`.

Let\'s look at some instances.

~~~~~{.haskell}
instance Semigroup [a] where
  (<>) = (++) 

instance Monoid [a] where
  mempty  = []
  -- mappend = (<>) 
  mconcat xss = [ x | xs <- xss, x <-xs ] 
~~~~~~~~~~

Lists are the canonical example of a `Monoid` \-\- they can be combined together with
`(++)`, and the empty list, when combined with any other list via `(++)`, gives that other list as a result.
This instance of `Monoid` is in the standard library \-\- so you don\'t actually need to use the specialized
`(++)` for lists; you can use the more general `(<>)` almost whenever you please.

Furthermore, the test case below demonstrates that lists satisfy the required properties of monoids:
the empty list is a left and right identity for append, and concatenation is an associative operation.

> testListMonoid :: Test
> testListMonoid =
>   let t1 = [1,2] in
>   let t2 = [3,4] in
>   let t3 = [1,2,3,4] in
>   TestList [ mempty <> t1     ~?= t1,              -- left identity
>              t1 <> mempty     ~?= t1,              -- right identity
>              (t1 <> t2) <> t3 ~?= t1 <> (t2 <> t3) -- associativity
>             ]

`Maybe` can also be defined as an instance of `Monoid` as follows:

~~~~~{.haskell}
instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> b       = b
  a       <> Nothing = a
  Just a  <> Just b  = Just (a <> b)

instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing
~~~~~~~~~

What else? Another example that may jump to mind is numbers. Any integer can be added to any other,
with zero being the identity element. So you might expect that the standard library would have an instance
like this:

~~~~~{.haskell}
instance Semigroup Integer where
  (<>) = (+) 

instance Monoid Integer where
  mempty  = 0
~~~~~~~~~

But it does not. After all, you could just as well realize that integers can be combined by multiplying
them, with one being the identity element! In that case, we\'d write an instance like this:

~~~~~{.haskell}
instance Semigroup Integer where
  (<>) = (*) 

instance Monoid Integer where
  mempty  = 1
~~~~~~~~~

Who\'s to say which monoidal interpretation of integers is \"more right\"?

In cases like this, we usually use a newtype to differentiate between which interpretation we want to use.
That is, we can instead say:

~~~~~{.haskell}
newtype Sum     a = Sum     { getSum     :: a }
newtype Product a = Product { getProduct :: a }

instance Num a => Semigroup (Sum a) where
  x <> y = Sum $ getSum x + getSum y 

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

instance Num a => Semigroup (Product a) where
  x <> y = Product $ getProduct x * getProduct y 

instance Num a => Monoid (Product a) where
  mempty = Product 1
~~~~~~~~~~~

Notice that in the above, these `Monoid` instances require a `Num` instance for the types they\'re wrapping.
`Num`, as you have seen, is a typeclass which provides overloading for numeric operations and literals \-\-
so using it as a superclass of our `Monoid` instance allows us to be generic over what kind of numbers we\'re
manipulating, rather than fixing a particular type of number.

For example, we can calculate the sum and product of a list of integers by coercing the elements to type
`Sum Int` and `Product Int` respectively.

> ten :: Int
> ten = getSum (mconcat (map Sum [1,2,3,4]))
>
> twentyFour :: Int
> twentyFour = getProduct (mconcat (map Product [1,2,3,4]))

But given that we already have individual functions like  `++`, `+` and `*`, why would we ever want to use
`mappend` instead?

One reason is that with a monoid we get another function called `mconcat` for free. `mconcat` takes a list
of values in a monoid and combines them all together. For example `mconcat [a,b,c]` is equal to
`a mappend (b mappend c)`.
Any time you have a monoid you have this quick and easy way to combine a whole list together.
But note that there is some ambiguity in the idea behind `mconcat`. To compute `mconcat [a,b,...,c,d]`
which order should we work in? Should we work from left to right and compute `a mappend b` first?
Or should we start with `c mappend d`. That\'s one place where the associativity law comes in:
it makes no difference.

Another place where you might want to use a monoid is in code that is agnostic about how you want to
combine elements. Just as mconcat works with any monoid, you might want to write your own code that
works with any monoid.

Explicitly using the `Monoid` type class for a function also tells the reader of your code what your intentions are.
If a function has signature `[a] -> b` you know it takes a list and constructs an object of type `b from it.
But it has considerable freedom in what it can do with your list. But if you see a function of type
`(Monoid a) => a -> b`, even if it is only used with lists, we know what kind of things the function will
do with the list. For example, we know that the function might add things to your list, but it\'s never going
to pull any elements out of your list.


Foldable
=========

We have seen that `foldr` captures a common pattern of computation on lists \-\- you can think of it as
a functional for-loop.

~~~~~{.haskell}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f y [] = y
foldr f y (x:xs) = f x (foldr f y xs)
~~~~~~~~~~~

We can use it to define all kinds of simple list traversals. For example:

~~~~~{.haskell}
sum = foldr (+) 0
maximum = foldr max minBound xs
(++) = \ys -> foldr (:) ys xs
concat = foldr (++) []
map = \f -> foldr (xs -> f x : xs) []
~~~~~~~~~~~~

We have also seen that there are other datatypes that support some form of fold operator. For example:

>     data Tree a = Empty | Branch a (Tree a) (Tree a)
>
>     foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
>     foldTree _ e Empty     = e
>     foldTree f e (Branch a n1 n2) = f a (foldTree f e n1) (foldTree f e n2)
>
>     sumTree :: Num a => Tree a -> a
>     sumTree = foldTree (+) 0
>
>     infixOrder :: Tree a -> [a]
>     infixOrder = foldTree (:) []

In fact, we can define folds over almost any data structure. This pattern of computation is abstracted
in typeclass `Foldable`. Much like Functor is for things that can be mapped over, `Foldable` is for things that can
be folded up!

What are some of the functions that this type class defines? Well, among them are `foldr`, `foldl`,
`foldr1`, and `foldl1`. Huh? We already know these functions. What\'s so new about this? Let\'s compare the types of
`Foldable`s `foldr` and `foldr` from `Prelude` to see how they differ (we have imported `Data.Foldable`qualified as `F`}:

~~~~~{.haskell}
ghci> :t foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
ghci> :t F.foldr
F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b
~~~~~~~~~~

Ah! So whereas `foldr` takes a list and folds it up, the `foldr` from `Data.Foldable` accepts any type that can be
folded up, not just lists! As expected, both `foldr` functions do the same for lists:

~~~~~{.haskell}
ghci> foldr (*) 1 [1,2,3]
6
ghci> F.foldr (*) 1 [1,2,3]
6
~~~~~~~~~~

One way to make a type constructor an instance of `Foldable` is to just directly implement `foldr` for it.
But another, often much easier way, is to implement the `foldMap` function, which is also a part of the
`Foldable` type class. The `foldMap` function has the following type:

~~~~~{.haskell}
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
~~~~~~~~~~~~

Its first parameter is a function that takes a value of the type `a` that our foldable structure contains,
and returns a monoid value. Its second parameter is a foldable structure that contains values of type `a`.
It maps that function over the foldable structure, thus producing a foldable structure that contains monoid values.
Then, by doing `mappend` (`<>`) between those monoid values, it joins them all into a single monoid value.

This function may sound kind of odd at the moment, but you\'ll see that it\'s very easy to implement.
And implementing this function is all it takes for our type to be made an instance of `Foldable`!
If we just implement `foldMap` for some type, we get `foldr` and `foldl` on that type for free.

This is how we make Tree an instance of `Foldable`:

> instance F.Foldable Tree where
>   foldMap f Empty          = mempty
>   foldMap f (Branch x l r) = F.foldMap f l <> f x (F.foldMap f r)

Notice that we didn\'t need to provide the function that takes a value and returns a monoid value.
We receive that function as a parameter to `foldMap`, and all we need to decide is where to apply that
function and how to join the resulting monoids from it.

Now that we have a Foldable instance for our tree type, we get `foldr` and `foldl` for free! Consider this tree:

> exTree :: Tree Int
> exTree = Branch 2 (Branch 3 Empty Empty) (Branch 1 (Branch 5 Empty Empty) Empty)

With a Foldable instance, we can do all of the folds that we can do on lists:

~~~~~{.haskell}
ghci> F.foldl (+) 0 exTree
11
ghci> F.foldl (*) 1 exTree
30
~~~~~~~~~~~

Function `foldMap` also comes in handy for reducing our structure to a single monoid value.
For instance, if we want to know if any number in our tree is equal to 3, we can do this:

~~~~~{.haskell}
ghci> getAny $ F.foldMap (\x -> Any $ x == 3) exTree
True
~~~~~~~~~~~

Here, `\x -> Any $ x == 3` is a function that takes a number and returns a monoid value: a `Bool` wrapped in `Any`.
`foldMap` applies this function to every element in our tree and then reduces the resulting monoids into a single
monoid with `mappend`. Suppose we do this:

We can also easily turn our tree into a list by doing a `foldMap \x -> [x]`. By first projecting that function onto
our tree, each element becomes a singleton list. The `mappend` action that takes place between all those singleton
lists results in a single list that holds all of the elements that are in our tree:

~~~~~{.haskell}
ghci> F.foldMap (\x -> [x]) exTree
[3,2,5,1]
~~~~~~~~~

What\'s cool is that all of these tricks aren\'t limited to trees. They work on any instance of `Foldable`!


Alternative
============

Looking at the mathematical definition of a *monoid*, we see that it says nothing about *combining*,
but only state the properties that must be satisfied by operators `<>` and `mempty`. Now, it’s certainly
true that combining things works well with this structure: `mempty` corresponding to having no things,
and `m1 <> m2` saying that if I glom `m1` and `m2`’s stuff together, I can get a new thing containing
all their stuff. But here’s an alternative intuition: `empty` corresponds to no choices at all,
and `m1 <> m2` corresponding to a choice between `m1` and `m2`. Note that both interpretations obey the monoid
laws: 

 @ Having nothing at all and having no choice are both the identity.

  * If I have no stuff and glom it together with some stuff, I end up with that same stuff again.
  * If I have a choice between no choice at all (something impossible) and some other choice, I have
    to pick the other (possible) choice.

 @ Glomming collections together and making a choice are both associative.

  * If I have three collections of things, it doesn’t matter if I glom the first two together and then
    the third, or the last two together and then the first; either way, I end up with the same total glommed collection.

  * If I have a choice between three things, it doesn’t matter if I (a) first choose between
    first-or-second and third and then, if I need to, between first and second, or (b) first
    choose between first and second-or-third and then, if I need to, between second and third.
    Either way, I can pick what I want.

*Note*: It’s important to remember that `<>` need not be commutative: it’s perfectly possible that
`m1 <> m2` $\not=$ `m2 <> m1`.

The `Alternative` type class is another type class that represents objects which (a) are applicative
functors, and (b) when instantiated at a type, have a value and a binary function on them which follow
the monoid rules -- it intends to capture the *choice* interpretation for the monoidal operations.

~~~~~~~ {.haskell}
class Applicative f => Alternative f where
  -- | The identity of '<|>'
  empty :: f a
  
  -- | An associative binary operation
  (<|>) :: f a -> f a -> f a
  
  -- | One or more. The definition of some is optional
  some :: f a -> f [a]
  some v = some_v
    where many_v = some_v <|> pure []
          some_v = liftA2 (:) v many_v

  -- | Zero or more. The definition is many is optional
  many :: f a -> f [a]
  many v = many_v
    where many_v = some_v <|> pure []
          some_v = liftA2 (:) v many_v
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As an example, `Maybe` is defined as an instance of `Alternative` as follows:

~~~~~ {.haskell}
instance Alternative Maybe where
  empty               = Nothing

  Nothing <|> m = m
  Just x  <|> _ = Just x 
~~~~~~~~~~~~~~~~~~~~~~~

Note that the above definition is equivalent to the following:

~~~~~~~~~ {.haskell} 
  Nothing <|> Nothing = Nothing -- 0 results + 0 results = 0 results
  Just x  <|> Nothing = Just x  -- 1 result  + 0 results = 1 result
  Nothing <|> Just x  = Just x  -- 0 results + 1 result  = 1 result
  Just x  <|> Just y  = Just x  -- 1 result  + 1 result  = 1 result:
~~~~~~~~~~~~~~~~~~~~~~

List is also an instance of the `Alternative` typeclass:

~~~~~~~~~ {.haskell}
instance Alternative [] where
  empty = []
  (<|>) = (++) -- length xs + length ys = length (xs ++ ys)
~~~~~~~~~~~~~~~~~~

Intuitively, lists are used here as representing nondeterministic choice: if I combine two values of type
`[a]` with `<|>`, that corresponds to nondeterministically picking either an action from the left or an action
from the right. But sometimes, you’re going to have no possible actions on one side \—\- and that’s fine. Similarly,
if we consider parsers, `<|>` represents a parser which parses either what’s on the left or what’s on the right
(it *picks*). And if you have a parser which always fails, that ends up being an identity: if you pick it, you
immediately reject that pick and try the other one.


Reading
--------

  * Miran Lipovaca, *Learn You a Haskell for Great Good!*, chapter 8.
  
Futher reading
---------------

 * [Typeclassopedia][typeclassopedia] explains many of the type classes. 

 * [Haskell wikibook page on category theory][hwikicat],
 for reading about the category theory behind the concept of functors, applicatives and monads.
 
 * Brent Yorgey, [Monoids: Theme and Variations (Functional Pearl)][Yorgey] uses functors and monoids in the
 definiton of a EDSL for drawing Diagrams.

[hwikicat]: https://en.wikibooks.org/wiki/Haskell/Category_theory
[typeclassopedia]: https://wiki.haskell.org/Typeclassopedia
[Yorgey]: http://ozark.hendrix.edu/~yorgey/pub/monoid-pearl.pdf