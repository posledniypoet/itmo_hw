{-# LANGUAGE InstanceSigs #-}
module HW1.T3
  ( Tree(..),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList
  )
where


data Tree a = Leaf | Branch Int (Tree a) a (Tree a) deriving (Show)
tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (size) left value right) = tsize(left) + tsize(right) + 1

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (size) Leaf value Leaf) = 1
tdepth (Branch (size) left value right) = tdepth(left) + tdepth(right) +1


tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember n (Branch (size) left value right)
        | n == value = True
        | n < value = tmember n left
        | n > value = tmember n right


tinsert :: Ord a => a -> Tree a -> Tree a
tinsert n Leaf = Branch 1 Leaf n Leaf
tinsert n (Branch (size) left value right)
          | (n > value)  = Branch (size) left value (tinsert n right)
          | (n < value) = Branch (size) (tinsert n left) value right
          | n == value = Branch (size) left value right

tcheckr :: Tree a -> Bool
tcheckr (Branch (size) (Branch (size1) left1 value1 right1) value Leaf) = True
tcheckr (Branch (size) left value right) = False

tcheckl :: Tree a -> Bool
tcheckl (Branch (size) Leaf value (Branch (size1) left1 value1 right1)) = True
tcheckl (Branch (size) left value right) = False

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf




