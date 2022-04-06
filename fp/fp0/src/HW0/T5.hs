module HW0.T5 (nz,
               ns,
               nplus,
               nmult,
               nFromNatural,
               nToNum
              ) where

import Numeric.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz s z = z

ns :: Nat a -> Nat a
ns n s z = s (n s z)

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus n m s z = n s (m s z)
nmult n m s = n (m s)

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = (\f x -> f (nFromNatural (n - 1) f x))

nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0