{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.LoadSU where

import Vinapu.Common (ro2dec)

data LoadSU = LoadSU {
                    service :: Double,
                    ultimate :: Double 
                } deriving Show

instance Eq LoadSU where
    (==) l1 l2 = slsL1 == slsL2 && ulsL1 == ulsL2
        where slsL1 = ro2dec (service l1) 2
              slsL2 = ro2dec (service l2) 2
              ulsL1 = ro2dec (ultimate l1) 2
              ulsL2 = ro2dec (ultimate l2) 2

(<+>) :: LoadSU -> LoadSU -> LoadSU 
(<+>) lr1 lr2 = LoadSU service' ultimate'
    where service' = (service lr1) + (service lr2)
          ultimate' = (ultimate lr1) + (ultimate lr2)

(<++>) :: Maybe LoadSU -> Maybe LoadSU -> Maybe LoadSU 
(<++>) Nothing lr2 = lr2
(<++>) lr1 Nothing = lr1
(<++>) (Just lr1) (Just lr2) = Just $ lr1 <+> lr2

infixl 6 <+> -- ^ Set associativity same as Num +
