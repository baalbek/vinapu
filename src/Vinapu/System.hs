{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.System where

import Text.Printf (printf)
import qualified Text.XML.Light as X 

import qualified Vinapu.Elements as E
import qualified Vinapu.Loads as L
import qualified Vinapu.Nodes as N
import qualified Vinapu.ElementResults as R
import qualified Vinapu.XML.XmlNodes as XN
import qualified Vinapu.XML.XmlLoads as XL
import Vinapu.Common (LimitState,partition)

type NodeSpan = [N.Node]

collectSpan :: [E.Element] -> NodeSpan -> R.ElementResult
collectSpan elements nodeSpan = R.ElementResult nra nrb
    where [na,nb] = nodeSpan 
          span = E.spans na nb
          spanned = filter span elements
          nra = R.NodeResult na spanned
          nrb = R.NodeResult nb spanned
          --loadA = sumNode spanned na
          --loadB = sumNode spanned nb
          --nra = R.NodeResult na loadA
          --nrb = R.NodeResult nb loadB

collectResults :: [E.Element] ->  [NodeSpan] -> [R.ElementResult]
collectResults elements nodeSpans = 
    let collectSpan' = collectSpan elements 
    in  map collectSpan' nodeSpans

runVinapu :: [E.Element]
             -> [N.Node]
             -> IO ()
runVinapu elements nodes = 
    let nxp = partition 2 1 nodes 
        results = collectResults elements nxp in 
    mapM_ R.printElementResult results >>
    return ()

runVinapuXml :: X.Element -> IO ()
runVinapuXml doc = do
    let nodes = XN.createVinapuNodes doc
    let loads = XL.createVinapuLoads doc
    putStrLn $ show nodes
    putStrLn $ show loads 
    return ()
