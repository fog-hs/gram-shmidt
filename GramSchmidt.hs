{-#
Language
FlexibleContexts,
MonoLocalBinds
#-}

module GramSchmidt 
 (gramSchmidt 
 ,gramSchmidtPattern 
  )where

import Vec

gramSchmidt :: VectorP n => [VecP n] -> [VecP n]
gramSchmidt = reverse . foldl (\ys x -> f x ys:ys) []
 where
  f x = direction . foldl (g x) x
  g x a b =  a - project x b

gramSchmidtPattern :: ([a] -> c) -> ([a] -> c -> [b]) -> (a -> b -> a) -> [[a]] -> [c]
gramSchmidtPattern f1 f2 f3 = reverse . foldl (\ys x -> f x ys:ys) []
 where
  f x = f1  . foldl (g x) x
  g x a b = zipWith f3 a (f2 x b)


