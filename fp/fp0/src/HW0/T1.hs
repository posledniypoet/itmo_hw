{-# LANGUAGE TypeOperators #-}
module HW0.T1   (type (<->)(Iso),
                  flipIso,
                  runIso,
                  distrib,
                  assocPair,
                  assocEither
                )where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a) = (Left a, Left a)
distrib (Right (b,c)) = (Right b, Right c)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso left right where
  left (Left a) = Left (Left a)
  left (Right (Left b)) = Left (Right b)
  left (Right (Right c)) = Right c
  right (Left (Left a))  = Left a
  right (Left (Right b)) = Right (Left b)
  right (Right c)        = Right (Right c)