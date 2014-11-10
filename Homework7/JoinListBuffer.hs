{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import JoinList
import Scrabble
import Sized
import Buffer

instance Buffer (JoinList (Score, Size) String) where
  toString     = id
  fromString   = id
