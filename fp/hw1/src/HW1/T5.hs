{-# LANGUAGE InstanceSigs #-}
module HW1.T5
  ( splitOn,
    joinWith
  )
where

import Data.List.NonEmpty
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep str = foldr func ([] :| []) str
         where
              func cur (h :| t)
                 | cur == sep = [] :| (h : t)
                 | otherwise = (cur : h) :| t

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep lst = foldl1 join lst
         where
           join cur lst = cur ++ (sep:lst)

