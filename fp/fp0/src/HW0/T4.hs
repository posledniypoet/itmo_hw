module HW0.T4(repeat',
              map',
              fib,
              fac) where

import Data.Function (fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' = fix (\map'' f xs -> case xs of
                           []   -> []
                           t:h -> f t : map'' f h)

fib :: Natural -> Natural
fib = fix (\f fst snd n -> if n == 0 then fst
                           else f snd (fst + snd) (n - 1)) 0 1

fac :: Natural -> Natural
fac = fix (\f n -> if n <= 1 then 1
                   else n * f (n - 1))