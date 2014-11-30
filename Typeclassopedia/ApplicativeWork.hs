{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -XDataKinds -fno-warn-orphans #-}
module ApplicativeWork where

import Control.Applicative

-- Prove that
--    pure f <*> x = pure (flip ($)) <*> x <*> pure f

-- Prelude> :t flip ($)
-- flip ($) :: b -> (b -> c) -> c

-- pure (flip ($)) <*> x
-- (b -> c) -> c

-- pure (flip ($)) <*> x <*> pure f
-- c 

-- pure f <*> x 
-- c

-- equal

-- Make the Applicative Instance for Maybe

data Option a = Some a | None deriving Show

instance Applicative Option where
   pure x                = Some x
   (Some f) <*> (Some x) = Some (f x) 
   _ <*> _               = None  