{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# OPTIONS_GHC -Wall #-}
import Control.Applicative

newtype ZL a = ZL { getZL :: [a] }
  deriving (Eq, Show, Functor)

instance Applicative ZL where
	pure            = ZL . repeat
	ZL fs <*> ZL xs = ZL (zipWith ($) fs xs) 

data BigRecord = BR { getName         :: Name,
                    , getSSN          :: String
                    , getSalary       :: Integer
                    , getPhone        :: String
                    , getLicensePlate :: String
                    , getNumSickDays  :: Int
                    }	