{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Dep where

import Prelude hiding (tail, head, replicate, min, length)

data Nat = Zero | Suc Nat

infixl 6 :+
infixl 7 :*

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Zero     :+ m = m
type instance (Suc n)  :+ m = Suc (n :+ m)

type family   (n :: Nat) :* (m :: Nat) :: Nat
type instance Zero     :* m = Zero
type instance (Suc n)  :* m = (n :* m) :+ m

min :: Nat -> Nat -> Nat
min Zero    Zero    = Zero
min Zero    (Suc _) = Zero
min (Suc _) Zero    = Zero
min (Suc m) (Suc n) = Suc (min m n)

data Vec a n where
  Nil  :: Vec a Zero
  (:>) :: a -> Vec a n -> Vec a (Suc n)
infixr 5 :>

deriving instance Eq a => Eq (Vec a n)

toList :: Vec a n -> [a]
toList Nil = []
toList (x :> xs) = x : toList xs

head :: Vec a (Suc n) -> a
head (x :> _) = x

tail :: Vec a (Suc n) -> Vec a n
tail (_ :> xs) = xs

append :: Vec a n -> Vec a m -> Vec a (n :+ m)
append (x :> xs) ys = x :> append xs ys
append Nil       ys = ys

instance Show a => Show (Vec a n) where
  showsPrec d = showsPrec d . toList

data SNat n where
  SZ :: SNat Zero
  SS :: SNat n -> SNat (Suc n)

deriving instance Show (SNat n)
deriving instance Eq (SNat n)

replicate :: SNat n -> a -> Vec a n
replicate SZ     _ = Nil
replicate (SS n) a = a :> replicate n a

length :: Vec a n -> SNat n
length Nil = SZ
length (_ :> v ) = SS (length v)


-- /show
main :: IO ()
main = do
  putStr $ "head (1 :> 2 :> Nil) == " 
  print $  head (1 :> 2 :> Nil)
  putStr $ "tail (1 :> 2 :> Nil) == " 
  print $ tail (1 :> 2 :> Nil)
  -- | Uncommenting the line below causes type error
  -- print $ head Nil
  putStr $ "append (1 :> (3 :> Nil)) (2 :> Nil) == " 
  print $ append (1 :> (3 :> Nil)) (2 :> Nil)
  putStr "replicate (SS (SS (SS SZ))) () == "
  print $ replicate (SS (SS (SS SZ))) ()
  putStr $ "length (1 :> 2 :> Nil) == " 
  print $  length (1 :> 2 :> Nil)

