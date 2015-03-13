{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}

module Vinapu.XML.XmlNodes where

import Control.Monad (mplus)
import qualified Data.Map as Map
import qualified Text.XML.Light as X 
import qualified Vinapu.Nodes as N
import qualified Vinapu.XML.Common as XC

type NodeDef = (String,N.Node)

genNodeDef :: X.Element -> NodeDef
genNodeDef el = (nid',N.Node nid' xcoord 0.0 desc)
    where nid = XC.xmlAttr "id" el 
          Just nid' = nid
          Just xcoord = XC.xmlAttr "x" el >>= Just . read :: Maybe Double
          Just desc = mplus (XC.xmlAttr "d" el) nid


genNodeDefs :: [X.Element] -> [NodeDef]
genNodeDefs elx = map genNodeDef elx

createVinapuNodes :: X.Element -> N.NodeMap
createVinapuNodes doc = Map.fromList nodeDefs 
    where nodeDefs = genNodeDefs (XC.xmlElements "node" doc)


