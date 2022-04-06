module HW1.T2
  ( N (..),
    nplus,
    nmult,
    ncmp,
    nFromNatural,
    nToNum,
    nOdd,
    nEven,
    ndiv,
    nmod,
    nsub
  )
where
import Numeric.Natural
data N = Z | S N deriving (Show)

nplus :: N -> N -> N
nplus Z n = n
nplus n Z = n
nplus (S n') n = S (nplus n' n)

nmult :: N -> N -> N
nmult _ Z = Z
nmult Z _ = Z
nmult (S Z) n = n
nmult n (S Z) = n
nmult (S n') n = nplus n (nmult n n')

nsub :: N -> N -> Maybe N
nsub n Z = Just n
nsub Z _ = Nothing
nsub (S n) (S n') = nsub n n'

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp _ Z = GT
ncmp Z _ = LT
ncmp (S n) (S n') = ncmp n n'

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = ((nToNum n) + 1)


nEven :: N -> Bool
nEven Z = True
nEven (S n) = not (nEven n)

nOdd :: N -> Bool
nOdd n = not (nEven n)

ndiv :: N -> N -> N
ndiv _ Z = error"2-nd argument must be > 0"
ndiv a b =
  if (ncmp a b == LT) then Z
  else if (ncmp a b == GT) then (S (ndiv (fromMaybe(nsub a b)) b))
  else (S Z)

fromMaybe :: Maybe a -> a
fromMaybe (Just a) = a
fromMaybe Nothing  = error"That's impossible"

nmod :: N -> N -> N
nmod a b =
  if (ncmp a b == LT) then a
  else if (ncmp a b == GT) then fromMaybe(nsub a (nmult (ndiv a b) b))
  else Z



