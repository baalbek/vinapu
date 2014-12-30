{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.System where

import Text.Printf (printf)

import Vinapu.LoadSU ((<++>),LoadSU(..))
import qualified Vinapu.Elements as E
import qualified Vinapu.Loads as L
import qualified Vinapu.Nodes as N
import qualified Vinapu.ElementResults as R
import Vinapu.Common (LimitState,partition)

type NodeSpan = [N.Node]

-- | Node sums for node n for all elements in elx 
sumNode :: [E.Element] 
           -> N.Node 
           -> Maybe LoadSU
sumNode elx n = let latn = E.unitLoadAtNode n in foldr (<++>) Nothing $ map latn elx

collectSpan :: [E.Element] -> NodeSpan -> R.ElementResult
collectSpan elements nodeSpan = undefined 
    where [na,nb] = nodeSpan 

collectResults :: [E.Element] ->  [NodeSpan] -> [R.ElementResult]
collectResults elements nodeSpans = map collectSpan' nodeSpans
    where collectSpan' = collectSpan elements

runVinapu :: [E.Element]
             -> [N.Node]
             -> IO ()
runVinapu elements nodes = 
    let nxp = partition 2 1 nodes 
        results = collectResults elements nxp in 
    putStrLn (show nxp)
    >> return ()
    

{-
sys :: [Element] -> [N.Node] -> [Double]
sys elx nx = map (\x -> serviceLoadsIf elx (head x) (head (drop 1 x))) partnx
    where partnx = partition 2 1 nx
-}
          
