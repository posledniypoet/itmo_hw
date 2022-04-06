module HW1.T7
   ( ListPlus(..),
     Inclusive(..),
     DotString(DS),
     Fun(F)
   )where

data ListPlus a = a :+ ListPlus a | Last a
infixr 5 :+
data Inclusive a b = This a | That b | Both a b
newtype DotString = DS String deriving (Show)
newtype Fun a = F (a -> a)


instance Semigroup (ListPlus a) where
     (<>) (Last a) b = a :+ b
     (<>) (a :+ b) c = a :+ (b <> c)


instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
     (<>) (This a) (This b) = (This (a<>b))
     (<>) (This a) (That b) = (Both a b)
     (<>) (This a) (Both b c) = (Both (a<>b) c)
     (<>) (That a) (This b) = (Both b a)
     (<>) (That a) (That b) = (That (a<>b))
     (<>) (That a) (Both b c) = (Both b (a<>c))
     (<>) (Both a b) (This c) = (Both (a<>c) b)
     (<>) (Both a b) (That c) = (Both a (b<>c))
     (<>) (Both a b) (Both c d) = (Both (a<>c) (b<>d))

instance Semigroup DotString where
     (<>) (DS "") a = a
     (<>) b (DS "") = b
     (<>) (DS a) (DS b) = DS (a ++ "." ++ b)

instance Monoid DotString where
    mempty = DS ""

instance Semigroup (Fun a) where
    (<>) (F b) (F c) = (F (b . c))


instance Monoid (Fun a) where
     mempty = (F id)