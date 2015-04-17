{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}
module Vinapu.ElementResults where

import Vinapu.LoadSU ((<++>),LoadSU(..))
import qualified Vinapu.Nodes as N
import qualified Vinapu.Elements as E

data NodeResult = 
    NodeResult {
        node :: N.Node,
        spanned :: [E.Element]
    } deriving Show

data ElementResult = 
    ElementResult {
        nr1, nr2 :: NodeResult
    } deriving Show


-- | Node sums for node n for all elements in elx 
sumNode :: [E.Element] 
           -> N.Node 
           -> Maybe LoadSU
sumNode elx n = let latn = E.unitLoadAtNode n in foldr (<++>) Nothing $ map latn elx

