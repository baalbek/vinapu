{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}

module Vinapu.XML.XmlElements where

import qualified Data.Map as Map

import qualified Text.XML.Light as X 
import qualified Vinapu.Elements as E
import qualified Vinapu.Loads as L
import qualified Vinapu.Nodes as N
import qualified Vinapu.XML.Common as XC

getVinapuNode :: N.NodeMap 
                 -> X.Element
                 -> String
                 -> N.Node
getVinapuNode nm el nodeName = result
    where Just result = XC.xmlAttr nodeName el >>= (\s -> Map.lookup s nm) :: Maybe N.Node
 
getLoadPair :: L.LoadMap
               -> X.Element
               -> L.LoadPair
getLoadPair lm el = result
    where getLoadFor loadName = XC.xmlAttr loadName el >>= (\s -> Map.lookup s lm) :: Maybe L.DistLoad
          Just dload = getLoadFor "dload" 
          Just lload = getLoadFor "lload" 
          result = L.LoadPair dload lload
 
createElement :: N.NodeMap 
              -> L.LoadMap 
              -> X.Element 
              -> E.Element
createElement nm lm el = result
    where Just desc = XC.xmlAttr "desc" el 
          -- Just desc = mplus (XC.xmlAttr "desc" el) (Just eid)
          f = XC.a2d el "f"
          w = XC.a2d el "w"
          n1 = getVinapuNode nm el "n1"
          n2 = getVinapuNode nm el "n2"
          Just etype = XC.xmlAttr "type" el 
          result = case etype of 
            "plate" -> E.PlateElement n1 n2 w (getLoadPair lm el) f desc

createVinapuElements :: X.Element 
                        -> N.NodeMap 
                        -> L.LoadMap 
                        -> String
                        -> [E.Element]
createVinapuElements doc nm lm curCase = map createElement' curElements 
    where curElements = XC.xmlElements "element" (head (XC.xmlElements curCase doc))
          createElement' = createElement nm lm

loadCaseNode :: X.Element -> String -> X.Element
loadCaseNode doc curCase = lcNode
    where Just lcNodes = XC.xmlElement "load-cases" doc
          lcNode = head (XC.xmlElements "lc" lcNodes)
{-
testme :: X.Element -> String 
testme doc = etype 
    where dc = head $ XC.xmlElements "default-case" doc
          elx = head $ XC.xmlElements "element" dc
          Just etype = XC.xmlAttr "type" elx
          -}
