{-# LANGUAGE InstanceSigs #-}
module HW1.T6
  ( mcat,
    epart
  )
where 
import Data.Monoid
mcat :: Monoid a => [Maybe a] -> a
mcat lst = foldr func (mempty) lst
     where 
       func Nothing x = mappend mempty x
       func (Just cur) x = mappend cur x
       
       
       
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart lst = foldr func (mempty,mempty) lst
      where
        func (Right cur) (ml,mr) = (ml,mappend cur mr)
        func (Left cur) (ml,mr) = (mappend cur ml, mr)