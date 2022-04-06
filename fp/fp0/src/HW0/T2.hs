module HW0.T2 (
doubleNeg,
reduceTripleNeg,
Not
)where

import Data.Void (Void)

type Not a = a -> Void

doubleNeg :: a -> Not (Not a)
doubleNeg a f = f a

reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg = (. doubleNeg)