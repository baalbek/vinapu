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


data HtmlRow = 
    HtmlRow {
        titleCol :: String,
        serviceCol :: String,
        ultimateCol :: String
    } 
    | EmptyHtmlRow deriving Show
        
data HtmlTable =
    HtmlTable {
        title :: String,
        rows :: [HtmlRow]
    } deriving Show

htmlRow2str :: HtmlRow -> String
htmlRow2str row = undefined

htmlTable2str :: HtmlRow -> String
htmlTable2str table = undefined

-- | Node sums for node n for all elements in elx 
sumNode :: [E.Element] 
           -> N.Node 
           -> Maybe LoadSU
sumNode elx n = let latn = E.unitLoadAtNode n in foldr (<++>) Nothing $ map latn elx

sumLoad2str :: Maybe LoadSU -> String
sumLoad2str load = 
    case load of 
        Nothing -> "-"
        Just ld -> let sload = (LU.service ld)
                       uload = (LU.ultimate ld) in
                            printf "\t\tBruksgrense: %.2f kN/m, bruddgrense: %.2f kN/m (%.2f)" sload uload (uload / sload)

sumLoad2htmlRow :: Maybe LoadSU -> HtmlRow
sumLoad2htmlRow load =
    case load of 
        Nothing -> EmptyHtmlRow 
        Just ld -> let sload = (LU.service ld)
                       uload = (LU.ultimate ld) 
                       outFn x = printf "%.2f kN/m" x in HtmlRow "Sum" (outFn sload) (outFn uload)

loadPair2str :: Maybe L.LoadPair -> String
loadPair2str lp = 
    case lp of 
        Nothing -> "-"
        Just lp' -> printf "\t\t%s\n\t\t%s" (printLoad (L.deadLoad lp')) (printLoad (L.liveLoad lp'))
            where printLoad :: L.DistLoad -> String 
                  printLoad x = let lsu = L.loadSU1 x in printf "%s: bruksgrense %.2f kN/m2, bruddgrense %.2f kN/m2" (L.desc x) (LU.service lsu) (LU.ultimate lsu)
             

loadPair2htmlRow :: Maybe L.LoadPair -> HtmlRow
loadPair2htmlRow lp = 
    case lp of 
        Nothing -> EmptyHtmlRow 
        Just lp' -> HtmlRow "-" "-" "-"

printSpanned :: N.Node -> [E.Element] -> IO ()
printSpanned node spanned = 
    let printSpan el = putStr "\t" >> putStrLn (E.fullDesc el)  >> (putStrLn . loadPair2str . (E.loadPairAtNode node)) el in
        mapM_ printSpan spanned >> return ()

printNodeResult :: NodeResult -> IO ()
printNodeResult NodeResult { node,spanned} = 
    let nodeStr = printf "Node %s: " (N.nodeId node) 
        sumLoad = sumNode spanned node in 
    putStrLn nodeStr >>
    printSpanned node spanned >>
    putStrLn "\tSum:" >> putStrLn (sumLoad2str sumLoad) >> return ()


printElementResult :: ElementResult -> IO ()
printElementResult ElementResult { nr1,nr2 } =
    printNodeResult nr1 >>
    printNodeResult nr2 >>
    return ()

