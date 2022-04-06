{-# LANGUAGE FlexibleContexts #-}
module HW2.T1  
  ( -- * Datatypes
    Annotated (..)
  , Except (..)
  , Fun (..)
  , List (..)
  , Option (..)
  , Pair (..)
  , Prioritised (..)
  , Quad (..)
  , Stream (..)
  , Tree (..)
    -- * map functions
  , mapAnnotated
  , mapExcept
  , mapFun
  , mapList
  , mapOption
  , mapPair
  , mapPrioritised
  , mapQuad
  , mapStream
  , mapTree
  )  where

data Option a = None | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e

infix 0 :#

data Except e a = Error e | Success a

data Prioritised a = Low a | Medium a | High a

data Stream a = a :> Stream a

infixr 5 :>

data List a = Nil | a :. List a

infixr 5 :.

newtype Fun i a = F (i -> a)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None = None
mapOption f (Some a) = Some (f a)

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P x y) = P (f x) (f y)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q x y z w) = Q (f x) (f y) (f z) (f w)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (x :# y) =  (f x) :# y

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f (Error e) = Error e
mapExcept f (Success a) = Success (f a)

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (High a) = High (f a)
mapPrioritised f (Medium a) = Medium (f a)
mapPrioritised f (Low a) = Low (f a)

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (x :> y) = (f x) :> (mapStream f y)

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil = Nil
mapList f (x :. y) = (f x) :. (mapList f y)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F x) = F(f . x)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree f (Branch x y z) = Branch (mapTree f x) (f y) (mapTree f z)
