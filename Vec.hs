{-#
Language
FlexibleContexts
,TypeFamilies
,TypeApplications
,KindSignatures
,GADTs
,TypeSynonymInstances
,TypeOperators
,MultiParamTypeClasses
,RankNTypes
,TypeOperators
,ScopedTypeVariables
,ConstraintKinds
,FlexibleInstances,DataKinds
,AllowAmbiguousTypes
,UndecidableInstances
#-}

module Vec (module Vec,module List) where
import Nat
import List


----
-- Vec

type Vec n = List n Double

vec2 [a,b] = Cons a $ Cons b Empty

----
-- products and sizes

magnitude :: Vector n => Vec (S n) -> Double
magnitude x = sqrt (x `dot` x)

dot :: (Num (List n Double),Foldable (List (S n)),Applicative (List (S n))) => Vec (S n) -> Vec (S n) -> Double
dot a = sum . (a *)

l1 :: (Num (List n Double),Foldable (List (S n)),Applicative (List (S n))) => Vec (S n) -> Double
l1 = sum . abs

l2 :: (Num (List n Double),Foldable (List (S n)),Applicative (List (S n))) => Vec (S n) -> Double
l2 = sum . fmap (^2)

norm :: VectorP n => VecP n -> Double
norm = sqrt . l2

----
-- orientation

direction :: VectorP n => VecP n -> VecP n
direction xs = (1/norm xs) `scale` xs

project :: VectorP n => VecP n -> VecP n -> VecP n
project xs ys = ((xs `dot` ys) / (norm ys)^2) `scale` ys  

angle :: VectorP n => VecP n -> VecP n -> Double
angle a b = (a `dot` b) / ((norm a)*(norm b))

----
-- ZeroVector

class ZeroVector n where
 zeroVector :: Vec n

instance ZeroVector Z where
 zeroVector = Empty

instance ZeroVector n => ZeroVector (S n) where
 zeroVector = 0 `Cons` (zeroVector @n)

----
-- Num Vec

scale :: Vector n => Double -> Vec n -> Vec n
scale x = fmap (*x) 

instance Num (Vec 'Z) where
 (+) xs ys = Empty 
 (-) xs ys = Empty 
 (*) xs ys = Empty 
 negate = fmap negate
 abs = fmap abs
 fromInteger _ = Empty 

instance (Num (List n Double),Foldable (List (S n)),Functor (List (S n)),Applicative (List (S n))) => Num (Vec (S n)) where
 (+) xs ys = (+) <$> xs <*> ys
 (-) xs ys = (-) <$> xs <*> ys
 (*) xs ys = (*) <$> xs <*> ys
 negate = fmap negate
 abs = fmap abs
 fromInteger x = Cons (fromInteger x) (fromInteger x)

----
-- Num [Double] 

instance Num [Double] where
 (+) = zipWith (+)
 (*) = zipWith (*)
 fromInteger x = repeat (fromInteger x)

instance Fractional [Double] where
 (/) = zipWith (/)

----
-- Helper Constraint packets

type Vector' n = (UnzipList n,GetNat n,ToList n,FromList n,ZeroVector n,Num (List n Double),Functor (List n),Foldable (List n),Applicative (List n))

type Vector n = (Vector' n,Vector' (S n))

type VectorP n = (Vector (S n),Vector n)
type VecP n = Vec (S n)

type ListP n = List (S n) 