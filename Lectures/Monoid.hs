{-# OPTIONS_GHC -Wall -XGeneralizedNewtypeDeriving #-}
import Data.Monoid

newtype MySum a = MySum a
   deriving (Eq, Ord, Num, Show)

getMySum :: MySum a -> a
getMySum (MySum a) = a

instance Num a => Monoid (MySum a) where
	mempty  = MySum 0
	mappend = (+)

newtype MyProduct a = MyProduct a
   deriving (Eq, Ord, Num, Show)

getMyProduct :: MyProduct a -> a
getMyProduct (MyProduct a) = a

instance Num a => Monoid (MyProduct a) where
    mempty  = 1
    mappend = (*) 
