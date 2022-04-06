module HW1.T4
   (tfoldr,
    treeToList
   ) where

import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ n Leaf = n
tfoldr func n (Branch _ left value right) = tfoldr func (func value (tfoldr func n right)) left

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []