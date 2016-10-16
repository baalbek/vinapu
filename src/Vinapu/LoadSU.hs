{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.LoadSU where

import Data.Monoid (Monoid,mempty,mappend)

import Vinapu.Common (ro2dec)

data LoadSU = 
    LoadSU {
        service :: Double,
        ultimate :: Double
    } 
    deriving Show


instance Eq LoadSU where
    (==) l1 l2 = slsL1 == slsL2 && ulsL1 == ulsL2
        where slsL1 = ro2dec (service l1) 2
              slsL2 = ro2dec (service l2) 2
              ulsL1 = ro2dec (ultimate l1) 2
              ulsL2 = ro2dec (ultimate l2) 2

instance Monoid LoadSU where
    mempty = LoadSU 0.0 0.0
    mappend (LoadSU s1 u1) (LoadSU s2 u2) = LoadSU (s1 + s2) (u1 + u2) 

