{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}
module Vinapu.ElementResults where

import Text.Printf (printf)

import Vinapu.LoadSU ((<++>),LoadSU(..))
import qualified Vinapu.Nodes as N
import qualified Vinapu.Elements as E
import qualified Vinapu.Loads as L
import qualified Vinapu.LoadSU as LU

data NodeResult = NodeResult {
                node :: N.Node,
                -- load :: Maybe L.LoadSU
                spanned :: [E.Element]
            } deriving Show

data ElementResult = ElementResult {
                nr1, nr2 :: NodeResult
            } deriving Show

-- | Node sums for node n for all elements in elx 
sumNode :: [E.Element] 
           -> N.Node 
           -> Maybe LoadSU
sumNode elx n = let latn = E.unitLoadAtNode n in foldr (<++>) Nothing $ map latn elx

loadStr :: Maybe LoadSU -> String
loadStr load = 
    case load of 
        Nothing -> "-"
        Just ld -> let sload = (LU.service ld)
                       uload = (LU.ultimate ld) in
                            printf "\t\tService: %.2f kN/m, ultimate: %.2f kN/m" sload uload

loadPairStr :: Maybe L.LoadPair -> String
loadPairStr lp = 
    case lp of 
        Nothing -> "-"
        Just lp' -> printf "\t\t%s\n\t\t%s" (printLoad (L.deadLoad lp')) (printLoad (L.liveLoad lp'))
            where printLoad :: L.DistLoad -> String 
                  printLoad x = let lsu = L.loadSU1 x in printf "%s: service %.2f kN/m2, ultimate %.2f kN/m2" (L.desc x) (LU.service lsu) (LU.ultimate lsu)
             

printNodeResult :: NodeResult -> IO ()
printNodeResult NodeResult { node,spanned} = 
    let nodeStr = printf "\tNode %s: " (N.nodeId node) 
        sumLoad = sumNode spanned node  
        lp = E.loadPairAtNode node (head spanned) in
    putStrLn nodeStr >>
    putStrLn (loadPairStr lp) >>
    putStrLn (loadStr sumLoad) >> return ()

{-
printNodeResult :: NodeResult -> IO ()
printNodeResult NodeResult { node,load } = 
    let nodeStr = printf "\tNode %s: " (N.nodeId node) in 
    putStr nodeStr >>
    putStrLn (loadStr load) >> return ()
-}
    

printElementResult :: ElementResult -> IO ()
printElementResult ElementResult { nr1,nr2 } =
    putStrLn "Element Result:" >>
    printNodeResult nr1 >>
    printNodeResult nr2 >>
    return ()

