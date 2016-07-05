{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}

module Vinapu.XML.XmlLoads where

import qualified Data.Map as Map
import qualified Text.XML.Light as X 
import qualified Vinapu.Loads as L
import Vinapu.Loads (people,concreteSlab,ytong)
import qualified Vinapu.XML.Common as XC

type LoadDef = (Int,L.DistLoad)

genLoadDef :: X.Element -> LoadDef
genLoadDef el = (lid', load)
    where Just lid = XC.xmlAttr "id" el
          lid' = read lid :: Int
          Just ltype = XC.xmlAttr "type" el 
          load = case ltype of 
            "dload" -> let Just desc = XC.xmlAttr "desc" el in L.UniformDistLoad 0 L.DEAD_LOAD desc (XC.a2d el "qm2") (XC.a2d el "lf") 
            "snow" -> L.Snow (XC.a2d el "qm2") (XC.a2d el "ff") "Snølast"
            "people" -> people
            "ytong" -> ytong (XC.a2d el "t")
            "cslab" -> concreteSlab (XC.a2d el "t")
                         

createVinapuLoads :: X.Element -> L.LoadMap2
createVinapuLoads doc = Map.fromList loadDefs 
    where loadDefs = map genLoadDef (XC.xmlElements "load" doc)


