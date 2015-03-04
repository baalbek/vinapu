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
                            printf "\t\tService: %.2f kN/m, ultimate: %.2f kN/m (%.2f)" sload uload (uload / sload)

loadPairStr :: Maybe L.LoadPair -> String
loadPairStr lp = 
    case lp of 
        Nothing -> "-"
        Just lp' -> printf "\t\t%s\n\t\t%s" (printLoad (L.deadLoad lp')) (printLoad (L.liveLoad lp'))
            where printLoad :: L.DistLoad -> String 
                  printLoad x = let lsu = L.loadSU1 x in printf "%s: service %.2f kN/m2, ultimate %.2f kN/m2" (L.desc x) (LU.service lsu) (LU.ultimate lsu)
             

printSpanned :: N.Node -> [E.Element] -> IO ()
printSpanned node spanned = 
    let printSpan el = putStr "\t" >> putStrLn (E.fullDesc el)  >> (putStrLn . loadPairStr . (E.loadPairAtNode node)) el in
        mapM_ printSpan spanned >> return ()

printNodeResult :: NodeResult -> IO ()
printNodeResult NodeResult { node,spanned} = 
    let nodeStr = printf "Node %s: " (N.nodeId node) 
        sumLoad = sumNode spanned node in 
        -- lload2dloadFrac = (LU.ultimate sumLoad) / (LU.service sumLoad) in 
    putStrLn nodeStr >>
    printSpanned node spanned >>
    putStrLn "\tSum:" >> putStrLn (loadStr sumLoad) >> return ()


printElementResult :: ElementResult -> IO ()
printElementResult ElementResult { nr1,nr2 } =
    -- putStrLn "Element Result:" >>
    printNodeResult nr1 >>
    printNodeResult nr2 >>
    return ()

