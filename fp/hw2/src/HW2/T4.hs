{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module HW2.T4   
  ( -- * Datatypes
    Expr (..)
  , Prim (..)
  , State (..)
    -- * map functions
  , eval
  , joinState
  , mapState
  , modifyState
  , wrapState
  )where

import Control.Monad
import HW2.T1 (Annotated (..), mapAnnotated)

newtype State s a = S {runS :: s -> Annotated s a}

mapState :: (a -> b) -> State s a -> State s b
mapState f S{runS = f1} = S{runS = \s -> mapAnnotated f (f1 s)}

wrapState :: a -> State s a
wrapState a = S (\i -> a :# i)

joinState :: State s (State s a) -> State s a
joinState s1 = S (\state -> let (s2 :# s3) = runS s1 state in runS s2 s3)

modifyState :: (s -> s) -> State s ()
modifyState f = S(\i -> () :# f i)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- ^ (+)
  | Sub a a      -- ^ (-)
  | Mul a a      -- ^ (*)
  | Div a a      -- ^ (/)
  | Abs a        -- ^ abs
  | Sgn a        -- ^ signum

data Expr =
  Val Double
  | Op (Prim Expr)

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)


eval :: Expr -> State [Prim Double] Double
eval (Val a) = pure a
eval (Op(Add x y)) = do
  x1 <- eval x
  y1 <- eval y
  modifyState(Add x1 y1 :)
  return (x1 + y1)
eval (Op(Sub x y)) = do
  x1 <- eval x
  y1 <- eval y
  modifyState(Sub x1 y1 :)
  return (x1 - y1)
eval (Op(Mul x y)) = do
  x1 <- eval x
  y1 <- eval y
  modifyState(Mul x1 y1 :)
  return (x1 * y1)
eval (Op(Div x y)) = do
  x1 <- eval x
  y1 <- eval y
  modifyState(Div x1 y1 :)
  return (x1 / y1)
eval (Op(Abs x)) = do
  x1 <- eval x
  modifyState(Abs x1 :)
  return (abs x1)
eval (Op (Sgn x)) = do
  x1 <- eval x
  modifyState(Sgn x1 :)
  return (signum x1)
