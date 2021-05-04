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
module Nat where
data Nat = S Nat | Z

type family (+) (n :: Nat) (m::Nat) ::Nat where
 (+) n Z = n
 (+) n (S m) = S (n + m)

class GetNat (n :: Nat) where
 getNat :: Int 

instance GetNat Z where
 getNat = 0 

instance GetNat n => GetNat (S n) where
 getNat = 1 + (getNat @n)
