{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.System where

import Text.Printf (printf)
import qualified Text.XML.Light as X 

import qualified Data.Map as Map

import qualified Vinapu.Elements as E
import qualified Vinapu.Loads as L
import qualified Vinapu.Nodes as N
import qualified Vinapu.ElementResults as R
import qualified Vinapu.Printers as P
import qualified Vinapu.XML.XmlNodes as XN
import qualified Vinapu.XML.XmlLoads as XL
import qualified Vinapu.XML.XmlElements as XE
import Vinapu.Common (LimitState,partition)

type NodeSpan = [N.Node]

collectSpan :: [E.Element] -> NodeSpan -> R.ElementResult
collectSpan elements nodeSpan = R.ElementResult nra nrb
    where [na,nb] = nodeSpan 
          span = E.spans na nb
          spanned = filter span elements
          nra = R.NodeResult na spanned
          nrb = R.NodeResult nb spanned

collectResults :: [E.Element] ->  [NodeSpan] -> [R.ElementResult]
collectResults elements nodeSpans = 
    let collectSpan' = collectSpan elements 
    in  map collectSpan' nodeSpans


runVinapu :: [E.Element]
             -> [N.Node]
             -> [P.Printer]
             -> IO ()
runVinapu elements nodes printers = 
    let nxp = partition 2 1 nodes 
        results = collectResults elements nxp in
    -- P.print P.StdoutPrinter results >> 
    -- P.print (P.HtmlPrinter "one.html") results >>
    mapM_ (P.print results) printers >>
    return ()

runVinapuXml :: X.Element 
                -> String  -- ^ Load Case
                -> [P.Printer]
                -> IO ()
runVinapuXml doc lc printers = do
    let loads = XL.createVinapuLoads doc
    let lcel = XE.loadCase doc lc
    let nodes = XN.createVinapuNodes lcel
    let elx = XE.createVinapuElements lcel nodes loads 
    runVinapu elx (Map.elems nodes) printers
    return ()

