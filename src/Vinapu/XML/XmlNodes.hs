{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}

module Vinapu.XML.XmlNodes where

import qualified Data.Map as Map
import qualified Text.XML.Light as X 
import qualified Vinapu.Nodes as N
import qualified Vinapu.XML.Common as XC

type NodeDef = (String,N.Node)

genNodeDefs :: [X.Element] -> Int -> [NodeDef]
genNodeDefs [] _ = []

createVinapuNodes :: X.Element -> N.NodeMap
createVinapuNodes doc = Map.fromList nodeDefs 
    where nodeDefs = genNodeDefs (XC.xmlElements "node" doc) 0


