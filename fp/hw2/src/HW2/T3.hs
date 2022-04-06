{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module HW2.T3   
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where

import HW2.T1 (Annotated (..), Except (..), Fun (..), List (..), Option (..), Pair (..),
               Prioritised (..), Quad (..), Stream (..))

joinOption    :: Option (Option a) -> Option a
joinOption (Some x) = x
joinOption None = None

joinExcept    :: Except e (Except e a) -> Except e a
joinExcept (Error x) = Error x
joinExcept (Success x) = x

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e) :# e') = a :# (e' <> e)

joinList      :: List (List a) -> List a
joinList (a :. b) = concatList2 a (joinList b)
joinList Nil = Nil

concatList2 :: List a -> List a -> List a
concatList2 Nil x = x
concatList2 (x :. y) l = x :. concatList2 y l

joinFun       :: Fun i (Fun i a) -> Fun i a
joinFun (F x) = F(\i -> let (F y) = x i
                            res = y i
                        in res)

