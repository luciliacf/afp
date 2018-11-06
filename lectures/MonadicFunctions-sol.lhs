---
title: "Exercise: General Monadic Functions"
date: Oct 25, 2017
---

> module MonadExercise where

> import Prelude hiding (mapM, foldM, sequence, ap)
> import Test.HUnit
> 
> import Data.Char (isUpper, toUpper, isAlpha, ord)
> 

Generic Monad Operations
========================

This problem asks you to recreate some of the operations in the
[Control.Monad](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Monad.html)
library. You should *not* use any of the functions defined in that library to
solve this problem.  (These functions also appear in more general forms
elsewhere, so other libraries that are off limits for this problem include
`Control.Applicative`, `Data.Traversable` and `Data.Foldable`.)

Add tests for each of these functions with at least two test cases, one using
the `Maybe` monad, and one using the `List` monad.


> -- (a) Define a monadic generalization of map

> mapM :: Monad m => (a -> m b) -> [a] -> m [b]
> 
> mapM _ []     = return []
> mapM f (x:xs) = do
>    b <- f x
>    bs <- mapM f xs
>    return (b:bs)
> 

> safeUpper :: Char -> Maybe Char
> safeUpper x = if isAlpha x then Just (toUpper x) else Nothing

> testMapM :: Test
> testMapM = TestList [
>    -- Maybe monad tests
>    mapM safeUpper "sjkdhf"  ~?= Just "SJKDHF",
>    mapM safeUpper "sa2ljsd" ~?= Nothing,
>    -- List monad tests
>    mapM (filter isUpper) ["QuickCheck", "Haskell"] ~?= ["QH", "CH"],
>    mapM (filter isUpper) ["AB", "monad"] ~?= []
>    ]
> 

> -- (b) Define a monadic generalization of foldl

> foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
> foldM _ a [] = return a
> foldM f a (b:bs) = do
>     a' <- f a b
>     foldM f a' bs 

> testFoldM :: Test
> testFoldM = TestList [ addEven [1,2,3]  ~=? Nothing,
>                        addEven [2,4]    ~=? Just 6,
>                        foldM safeDiv 16 [2,2] ~=? Just 4,
>                        foldM (flip replicate) 'a' [3,2] ~=? "aaaaaa"
>                      ]

> addEven :: [Int] -> Maybe Int
> addEven = foldM f 0 where
>                f x y | even x    = Just (x + y)
>                      | otherwise = Nothing

> safeDiv :: Int -> Int -> Maybe Int
> safeDiv x y = if y == 0 then Nothing else Just (x `div` y)


> crazy :: [Int] -> [Int]
> crazy = foldM f 0 where
>     f x y | even x    = [ x , y ]
>           | otherwise = [ y ]
> 

> -- (c) Define a generalization of monadic sequencing that evaluates
> -- each action in a list from left to right, collecting the results
> -- in a list.

> sequence :: Monad m => [m a] -> m [a]
> sequence = foldr k (return [])
>            where
>              k m m' = do { x <- m; xs <- m'; return (x:xs) } 

> testSequence :: Test
> testSequence = TestList [
>      sequence [Just 3, Nothing, Just 4] ~=? Nothing
>    , sequence [[1,2],[3],[4,5]] ~=? [[1,3,4],[1,3,5],[2,3,4],[2,3,5]]
>    , sequence (map safeUpper "abcd") ~=? Just "ABCD"
>    , sequence (map safeUpper "abcd2") ~=? Nothing
>    ]
 

> -- (d) Define the Kleisli "fish operator", a variant of composition

> (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
> f >=> g     = \x -> f x >>= g
> 

> testKleisli :: Test
> testKleisli = TestList [ (safeUpper >=> earlyOrd) 'a' ~=? Just 65
>                        , (safeUpper >=> earlyOrd) '2' ~=? Nothing
>                        , (mDup >=> mDup) 1 ~=? [1,1,1,1]
>                        , (replicate 2 >=> replicate 3) 'l' ~=? "llllll"
>                        ]

> mDup :: Int -> [Int]
> mDup x = [x,x]

> earlyOrd :: Char -> Maybe Int
> earlyOrd c = if c < 'm' then Just (ord c) else Nothing


For more information about this operator, see the explanation at the bottom of
[this page](http://www.haskell.org/haskellwiki/Monad_laws).

> -- (e) Define the 'join' operator, which removes one level of
> -- monadic structure.

> join :: (Monad m) => m (m a) -> m a
> join x            =  x >>= id

> testJoin :: Test
> testJoin = TestList [ join [[1,2],[3,4]] ~=? [1,2,3,4],
>                       join [[1,2],[3,4],[]] ~=? [1,2,3,4],
>                       join (Just (Just 3)) ~=? Just 3
>                     ] 

> -- (f) Define the 'liftM' function

Define `liftM`, which promotes functions `a -> b` to functions over
actions in a monad `m a -> m b`.

> liftM   :: (Monad m) => (a -> b) -> m a -> m b
> liftM f m1 = do { x1 <- m1; return (f x1) }
> 

> testLiftM :: Test
> testLiftM = TestList [ liftM not (Just True) ~=? Just False,
>                        liftM not [True,False] ~=? [False,True] ]

Thought question: Is the type of `liftM` similar to that of another
function we\'ve discussed recently?


ANSWER: `liftM` is the same function as `fmap`.


> -- (g) And its two-argument version ...

Now define a variation of `liftM`, `liftM2`, that works for
functions taking two arguments:

> liftM2  :: (Monad m) => (a -> b -> r) -> m a -> m b -> m r 
> liftM2 f m1 m2          = do { x1 <- m1; x2 <- m2; return (f x1 x2) }

> testLiftM2 :: Test
> testLiftM2 = TestList [liftM2 (+) (Just 1) (Just 2) ~=? Just 3,
>                        liftM2 (+) [1,2] [3,4] ~=? [4,5,5,6] ]
> 

> -- (h) Define the `ap` function

Define `ap`, which applies a monadic action to a monadic value, returning a monadic action.

> ap  :: (Monad m) => m (a -> b) -> m a -> m b
> ap m1 m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }

> testAp :: Test
> testAp = TestList [ [(*2), (^2)] `ap` [3,4] ~?= [6,8,9,16],
>                     [drop 2, take 2] `ap` [[1,2,3],[4,5],[6,6,6]] ~?= [[3],[],[6],[1,2],[4,5],[6,6]] ]


General Applicative Functions
=============================

Which of these functions above can you equivalently rewrite using `Applicative`?
i.e\. for which of the definitions below, can you replace `undefined` with
a definition that *only* uses members of the `Applicative` type class.
(Again, do not use functions from `Control.Applicative`, `Data.Foldable` or
`Data.Traversable` in your solution.)

If you provide a definition, you should write test cases that demonstrate that it
has the same behavior on `List` and `Maybe` as the monadic versions above.

> -- NOTE: you may not be able to define all of these, but be sure to test the
> -- ones that you do

> mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
> mapA f xs =  foldr ((<*>) . fmap (:) . f) (pure []) xs

> foldA :: Applicative f => (a -> b -> f a) -> a -> [b] -> f a
> foldA = undefined

> sequenceA :: Applicative f => [f a] -> f [a]
> sequenceA =  foldr (liftA2 (:)) (pure [])

> kleisliA :: Applicative f => (a -> f b) -> (b -> f c) -> a -> f c
> kleisliA = undefined

> joinA :: (Applicative f) => f (f a) -> f a
> joinA = undefined

> liftA   :: (Applicative f) => (a -> b) -> f a -> f b
> liftA f x = f <$> x

> liftA2  :: (Applicative f) => (a -> b -> r) -> f a -> f b -> f r
> liftA2 f x y = f <$> x <*> y