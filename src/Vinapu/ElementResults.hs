{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}
module Vinapu.ElementResults where

import qualified Vinapu.Nodes as N
import qualified Vinapu.LoadSU as L

data NodeResult = NodeResult {
                node :: N.Node,
                load :: L.LoadSU
            } deriving Show

data ElementResult = ElementResult {
                nr1, nr2 :: NodeResult
            } deriving Show
