---
title: What is Haskell?
author: Lucilia FIgueiredo
---

Haskell is an advanced [purely functional][functional],
[polymorphicaly statically typed][],  [lazy][lazy] language.

## Functional

Generally speaking, functional programming can be viewed as a _style_ of
programming in which the basic method of computation is the
application of functions to arguments: 

* Functions are first-class, that is, functions are values which can
  be used in exactly the same ways as any other sort of value.

* The meaning of Haskell programs is centered around evaluating
  expressions rather than executing instructions.

Taken together, these result in an entirely different way of thinking about programming. 

To illustrate this idea, let us consider the task of computing the
factorial of a given non-negative integer `n`.  In Java, this is usually
computed as follows:

~~~ .java
int factorial (int n) {
   int fact = 1
   for (int count = 1; count <= n; count ++)
       fact = fact * count 
   return fact }
~~~~~~~~~~~~~

In the above program, the basic method of computation is _changing
stored values_, in the sense that executing a program results in a
sequence of assignments. Languages such as Java, in which the basic
method of computation is changing stored values are called
_imperative_ languages, because programs in such languages are
constructed from imperative instructions that specify how computation
should proceed.

Now let us consider computing the factorial of `n` in
Haskell. this would normally be achieved by using two library
functions, one written as `[..]` that can be used to compute a list of
numbers between `1` and `n`, and the other called
`product`, that is used to compute the product of values in this
list:

~~~ .haskell
factorial n = product [1..n]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this program, the basic method of computation is _applying
functions to arguments_, in the sense that executing the program
results in a sequence of function applications.

Most imperative languages provide some support for programming with
functions, so the Haskell program above could be translated in such
languages. However, many imperative languages do not encourage
programming in functional style. For example, many such languages
discourage or prohibit functions to be stored in data structures such
as lists, taking fucntions as arguments or producing functions as
results. In constrast, Haskell imposes no such restrictions on how
functions can be used, and provides a number of features to make
programming with fucntions both simple and powerful.


## Pure 

Haskell expressions are always _referentially transparent_, that is:

* No mutation! Everything (variables, data structures etc) is immutable.
  
* Expressions never have *side effects* (like updating global variables or printing to the screen).
  
* Calling the same function with the same arguments results in the same output every time.

This may sound crazy at this point. How is it even possible to get
anything done without mutation or side effects? Well, it certainly
requires a shift in thinking (if you\'re used to an imperative or object-oriented paradigm). 
But once you\'ve made the shift, there are a number of wonderful benefits:

* Equational reasoning and refactoring: In Haskell one can always
  \"replace equals by equals\", just like you learned in algebra class.
  
* Parallelism: Evaluating expressions in parallel is easy when they
  are guaranteed not to affect one another.
  
* Fewer headaches: Simply put, unrestricted effects and action\-at\-a\-distance makes for programs 
that are hard to debug, maintain, and reason about.

Let\'s illustrate how side effects make it hard to reason about even the simplest, well-written code. 
To see this, you\'ll look at a collection of values, `myList`, and
reverse it by using built-in functionality. The following code is valid Python, Ruby, and JavaScript; 
see if you can figure out what it does.

~~~ .haskell
myList = [1,2,3]
myList.reverse()
newList = myList.reverse()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

What do you expect the value of `newList` to be? Because this is a
valid program in Ruby, Python, and JavaScript,  it seems reasonable to
assume that the value of `newList` 
should be the same. Here are the answers for all three languages:

Ruby ```-> [3,2,1]```

Python ```-> None```

JavaScript ```-> [1,2,3]```

Three completely different answers for the exact same code in three
languages! Python and JavaScript both have side effects that occur
when `reverse` is called. 
Because the side effects of calling `reverse` are different for
each  language and aren\’t made visible to the programmer,
both languages give different answers. 
The Ruby code here behaves like Haskell, without side effects. Here
you see the  value of referential transparency. 

## Polymorphicaly statically typed 

Most modern programming languages include some form of _type system_
to detect incompatibility errors, such as adding a number to a
character. Haskell has a type system that usually requires litle type
information from the programmer, but allows a large class of
incompatibility errors to be automatically detected prior to their
execution, using a sophisticated process called type inference.

The Haskell type sysytem is also more powerful than most languages,
supporting various forms of _polymorphism_ and _onverloading_, and
providing a wide range of special purpose features concerning types. 

## Lazy 

Haskell programs are executed using a technique called _lazy
evaluation_, which is based on the idea that no expression should be 
evaluated until its result is actually needed. This is a simple
decision with far\-reaching consequences. As well as avoiding
unnecessary computations, lazy evaluation ensures that programs
terminate whenever possible, encourages programming in a modular style
using intemediate data structures, and even allows programming with
infinite data structures.

As an example, the list of the first `n` Fibonnacci numbers can be
computed by the following Haskell program:

~~~ .haskel
fibs :: [Integer]
fibs = fibs' (0,1) 
   where fibs' (fib1,fib2) = fib1 : fibs' (fib2,fib1+fib2)

nFibs :: Integer -> [Integer]
nFibs n = take n fibs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that function `fibs` above computes the infinite list of
Fibonnaci numbers. Funtion `nFibs`, which returns the list of the
first ```n``` Fibonacci numbers, is defined modularly, by using this
infinite list and function `take`, which takes the first `n`
elements of a given list.

One major downside of _lazy evaluation_, however, is that reasoning about time and space 
usage becomes much more complicated!
