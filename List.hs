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

module List (module List,module Nat) where

import Nat

----
-- List

data List (n::Nat) a where
 Cons :: a -> List n a -> List (S n) a
 Empty :: List Z a

uncons :: List (S n) a -> (a,List n a)
uncons (a `Cons` b) = (a,b)

snoc :: ConcatList m ('S 'Z) => List m a -> a -> List ('S m) a
snoc xs x = xs +++ (Cons x Empty)

----
-- FromList

class FromList n where
 fromList :: List n a -> [a]

instance FromList Z where
 fromList Empty = []

instance FromList n => FromList (S n) where
 fromList (Cons x xs) = x : (fromList xs)

instance (Show a,FromList n) => Show (List n a) where
 show = show . fromList 


----
-- ToList

class ToList n where
 toList :: [a] -> List n a

instance ToList Z where
 toList [] = Empty

instance ToList n => ToList (S n) where
 toList (x:xs) = Cons x (toList xs)


----
-- ConcatList

class ConcatList n m where
 (+++) :: List n a -> List m a -> List (n+m) a

instance m ~ (Z + m) => ConcatList Z m where
 (+++) Empty ys = ys

instance m ~ (Z + m) => ConcatList n Z where
 (+++) ys Empty = ys

instance (S n + m ~ 'S (n + m),ConcatList n m) => ConcatList (S n) m where
 (+++) (Cons x xs) ys = x `Cons` (xs +++ ys)


----
-- UnzipList


class UnzipList n where
 unzipList :: List n (a,b) -> (List n a,List n b)

instance UnzipList Z where
 unzipList Empty = (Empty,Empty)

instance UnzipList n => UnzipList (S n) where
 unzipList (Cons (x,y) xs) = let (a,b) = unzipList @n xs in (x `Cons` a,y `Cons` b)


----
-- Functor List

instance Functor (List Z) where
 fmap f _ = Empty

instance Functor (List n) =>Functor (List (S n)) where
 fmap f (Cons x xs) = f x `Cons` fmap f xs

instance Foldable(List Z) where


----
-- Foldable List

 foldr f b xs = b

instance Foldable (List n) => Foldable (List (S n)) where
 foldr f b (Cons x xs) = f x (foldr f b xs)


----
-- Applicative List

instance Applicative (List Z) where
 pure = error "pure1"
 (<*>) Empty Empty = Empty

instance Applicative (List Z) => Applicative (List (S Z)) where
 pure = flip Cons Empty
 (<*>) (Cons f Empty) (Cons a Empty) = Cons (f a) Empty

instance Applicative (List (S n)) => Applicative (List (S (S n))) where
 pure = error "pure2"
 (<*>) (Cons f fs@(Cons _ _)) (Cons x xs@(Cons _ _)) = Cons (f x) ((<*>) fs xs)

