{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards  #-}
module Vinapu.Printers where

import Control.Monad (mplus)
import Text.Printf (printf)
import Vinapu.ElementResults (ElementResult(..),NodeResult(..),sumNode)
import qualified Vinapu.ElementResults as ER
import Vinapu.LoadSU (LoadSU(..))
import qualified Vinapu.Nodes as N
import qualified Vinapu.LoadSU as LU
import qualified Vinapu.Loads as L
import qualified Vinapu.Elements as E
import qualified Vinapu.Projects as PJ

type NodeResultPrinter = NodeResult -> IO ()

data Printer =
    StdoutPrinter 
    | HtmlPrinter {
        fileName :: String
    }
    deriving Show
   
sumLoad2str :: Maybe LoadSU -> String
sumLoad2str load = 
    case load of 
        Nothing -> "-"
        Just ld -> let sload = (LU.service ld)
                       uload = (LU.ultimate ld) in
                            printf "\t\tBruksgrense: %.2f kN/m, bruddgrense: %.2f kN/m (%.2f)" sload uload (uload / sload)

loadPair2str :: Maybe L.LoadPair -> String
loadPair2str lp = 
    case lp of 
        Nothing -> "-"
        Just lp' -> printf "\t\t%s\n\t\t%s" (printLoad (L.deadLoad lp')) (printLoad (L.liveLoad lp'))
            where printLoad :: L.DistLoad -> String 
                  printLoad (L.EmptyLoad) = "-"
                  printLoad x = let lsu = L.loadSU1 x in printf "%s: bruksgrense %.2f kN/m2, bruddgrense %.2f kN/m2" (L.desc x) (LU.service lsu) (LU.ultimate lsu)
                  --printLoad x@(L.UniformDistLoad _ _ _ _ _) = let lsu = L.loadSU1 x in printf "%s: bruksgrense %.2f kN/m2, bruddgrense %.2f kN/m2" (L.desc x) (LU.service lsu) (LU.ultimate lsu)
                  --printLoad x@(L.Snow _ _ _) = let lsu = L.loadSU1 x in printf "%s: bruksgrense %.2f kN/m2, bruddgrense %.2f kN/m2" (L.desc x) (LU.service lsu) (LU.ultimate lsu)

printSpanned :: N.Node -> [E.Element] -> IO ()
printSpanned node spanned = 
    let printSpan el = putStr "\t" >> putStrLn (E.fullDesc el)  >> (putStrLn . loadPair2str . (E.loadPairAtNode node)) el in
        mapM_ printSpan spanned >> return ()

printNodeResult :: NodeResult -> IO ()
printNodeResult NodeResult { node,spanned } = 
    let Just nodeDesc = mplus (N.desc node) (Just "N/A")
        nodeStr = printf "[%d] node %s: " (N.oid node) nodeDesc 
        sumLoad = sumNode spanned node in 
    putStrLn nodeStr >>
    printSpanned node spanned >>
    putStrLn "\tSum:" >> putStrLn (sumLoad2str sumLoad) >> return ()

printElementResult :: ElementResult -> IO ()
printElementResult ElementResult { nr1,nr2 } =
    printNodeResult nr1 >>
    printNodeResult nr2 >>
    return ()

htmlSpanned :: N.Node -> E.Element -> String
htmlSpanned node spanned = result
    where lp = E.loadPairAtNode node spanned
          printLoad :: L.DistLoad -> Bool -> String
          printLoad L.EmptyLoad _ = "<tr><td></td><td>-</td><td>-</td><td>-</td></tr>" 
          printLoad x isDeadLoad = 
            let lsu = L.loadSU1 x 
                serv = LU.service lsu 
                ult = LU.ultimate lsu in 
                case isDeadLoad of 
                    True -> printf "<tr><td>%s</td><td>(Egenlast) %s</td><td>%.2f</td><td>%.2f</td></tr>" (E.fullDesc spanned) (L.desc x) serv ult
                    False -> printf "<tr><td></td><td>(Nyttelast) %s</td><td>%.2f</td><td>%.2f</td></tr>" (L.desc x) serv ult
          result = case lp of 
                    Nothing -> "<tr><td>-</td><td>-</td><td>-</td><td>-</td></tr>"
                    Just lp' -> let deadLoad = L.deadLoad lp'
                                    liveLoad = L.liveLoad lp' in 
                                        printf "%s\n%s" (printLoad deadLoad True) (printLoad liveLoad False)

htmlNodeResult :: NodeResult -> [String]
htmlNodeResult NodeResult { node,spanned } = loads ++  [sumLoadHtml]
    where htmlSpanned' = htmlSpanned node
          loads = map htmlSpanned' spanned
          sumLoad = sumNode spanned node 
          sumLoadHtml = case sumLoad of
                        Nothing -> "<tr><td>Sum:</td><td/><td>0.0</td><td>0.0</td></tr>"  
                        Just sumLoad' -> printf "<tr><td>Total sum:</td><td/><td>%.1f</td><td>%.1f</td></tr>" (LU.service sumLoad') (LU.ultimate sumLoad')

htmlElementResult :: ElementResult -> [String]
htmlElementResult ElementResult { project, nr1, nr2 } = nodeHeader : loads1 
    where node1 = ER.node nr1
          node2 = ER.node nr2
          Just nodeDesc1 = mplus (N.desc node1) (Just "-")
          Just nodeDesc2 = mplus (N.desc node2) (Just "-")
          nodeHeader = case project of 
            Nothing -> printf "<tr><td><em><b>[%d] %s - [%d] %s</b></em></td><td/><td/><td/></tr>"  (N.oid node1) nodeDesc1 (N.oid node2) nodeDesc2
            Just pj -> printf "<tr><td><em><b>[%d] %s %s. Noder: [%d] %s - [%d] %s</b></em></td><td/><td/><td/></tr>" (PJ.sysId pj) (PJ.locName pj) (PJ.sysName pj) (N.oid node1) nodeDesc1 (N.oid node2) nodeDesc2
          loads1 = htmlNodeResult nr1
          -- loads2 = htmlNodeResult nr2

print :: [ElementResult] -> Printer -> IO ()
print elx StdoutPrinter = mapM_ printElementResult elx >> return ()
print elx HtmlPrinter { fileName } = 
    htmlPre >>= \html1  -> 
    htmlPost >>= \html2 -> 
        let body = unlines (concat (map htmlElementResult elx)) 
            result = printf "%s%s%s" html1 body html2 in
            writeFile fileName result

htmlPre :: IO String 
htmlPre = readFile "/home/rcs/opt/haskell/vinapu/resources/pre.html" 

htmlPost :: IO String 
htmlPost = readFile "/home/rcs/opt/haskell/vinapu/resources/post.html" 
