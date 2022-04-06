{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module HW2.T2   
  ( -- * dist functions
    distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
    -- * wrap functions
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
  ) where

import HW2.T1 (Annotated (..), Except (..), Fun (..), List (..), Option (..), Pair (..),
               Prioritised (..), Quad (..), Stream (..), mapList)

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption _ = None

wrapOption :: a -> Option a
wrapOption x = Some x

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P x y, P z w) = P (x, z) (y, w)

wrapPair :: a -> Pair a
wrapPair x = P x x

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q x y z w, Q c d e f) = Q (x, c) (y, d) (z, e) (w, f)

wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (x :# y, z :# w) = (x, z) :# (y <> w)

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success a, Success b) = Success (a, b)
distExcept (Error x, _) = Error x
distExcept (_, Error x) = Error x

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High x, High y) = High (x, y)
distPrioritised (High x, Medium y) = High (x, y)
distPrioritised (High x, Low y) = High (x, y)
distPrioritised (Medium x, High y) = High (x, y)
distPrioritised (Medium x, Medium y) = Medium (x, y)
distPrioritised (Medium x, Low y) = Medium (x, y)
distPrioritised (Low x, High y) = High (x, y)
distPrioritised (Low x, Medium y) = Medium (x, y)
distPrioritised (Low x, Low y) = Low (x, y)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (x :> y, z :> w) = (x, z) :> distStream (y, w)

wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (x :. y, l) = concatList (mapList (x,) l) $ distList (y, l)

wrapList :: a -> List a
wrapList x = x :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F a, F b) = F (\x -> (a x, b x))

wrapFun :: a -> Fun i a
wrapFun x = F (\i -> x)

concatList :: List a -> List a -> List a
concatList Nil x = x
concatList (x :. y) l = x :. concatList y l