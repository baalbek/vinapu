{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards  #-}
module Vinapu.Printers where

import Text.Printf (printf)
import Vinapu.ElementResults (ElementResult(..),NodeResult(..),sumNode)
import Vinapu.LoadSU ((<++>),LoadSU(..))
import qualified Vinapu.Nodes as N
import qualified Vinapu.LoadSU as LU
import qualified Vinapu.Loads as L
import qualified Vinapu.Elements as E
import qualified Vinapu.Tables as T

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
                  printLoad x = let lsu = L.loadSU1 x in printf "%s: bruksgrense %.2f kN/m2, bruddgrense %.2f kN/m2" (L.desc x) (LU.service lsu) (LU.ultimate lsu)
             

printSpanned :: N.Node -> [E.Element] -> IO ()
printSpanned node spanned = 
    let printSpan el = putStr "\t" >> putStrLn (E.fullDesc el)  >> (putStrLn . loadPair2str . (E.loadPairAtNode node)) el in
        mapM_ printSpan spanned >> return ()

printNodeResult :: NodeResult -> IO ()
printNodeResult NodeResult { node,spanned } = 
    let nodeStr = printf "[%d] node %s: " (N.oid node) "Node desc" --  (N.desc node) 
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
          printLoad x isDeadLoad = 
            let lsu = L.loadSU1 x 
                serv = LU.service lsu 
                ult = LU.ultimate lsu in 
                case isDeadLoad of 
                    True ->printf "<tr><td>%s</td><td>(Egenlast) %s</td><td>%.2f</td><td>%.2f</td></tr>" (E.fullDesc spanned) (L.desc x) serv ult
                    False ->printf "<tr><td></td><td>(Nyttelast) %s</td><td>%.2f</td><td>%.2f</td></tr>" (L.desc x) serv ult
          result = case lp of 
                    Nothing -> "<tr><td>-</td><td>-</td><td>-</td><td>-</td></tr>"
                    Just lp' -> let deadLoad = L.deadLoad lp'
                                    liveLoad = L.liveLoad lp' in 
                                        printf "%s\n%s" (printLoad deadLoad True) (printLoad liveLoad False)
                        

htmlNodeResult :: NodeResult -> [String]
htmlNodeResult NodeResult { node,spanned } =  (nodeStr : loads) ++  ["</table>"]
    where nodeStr = printf "<p>[%d] Node %s</p>\n<table>\n<tr><td>Element</td><td>Lasttype</td><td>Bruk</td><td>Brudd</td></tr>" (N.oid node) "Node desc" --  (N.desc node)
          htmlSpanned' = htmlSpanned node
          loads = map htmlSpanned' spanned
          sumLoad = sumNode spanned node 


htmlElementResult :: ElementResult -> [String]
htmlElementResult ElementResult { nr1,nr2 } = concat [(htmlNodeResult nr1),(htmlNodeResult nr2)]

print :: [ElementResult] -> Printer -> IO ()
print elx (StdoutPrinter) = mapM_ printElementResult elx >> return ()
--print elx HtmlPrinter { fileName } = putStrLn result -- writeFile fileName result
print elx HtmlPrinter { fileName } = writeFile fileName result
    where prefix = "<!DOCTYPE html>\n<html>\n<head>\n<title>Element Result</title>\n</head>\n<body>\n"
          postfix = "\n</body></html>"
          body = unlines (concat (map htmlElementResult elx)) 
          result = printf "%s%s%s" prefix body postfix

    -- mapM_ (printElementResult np) elx >> return ()
    --where np :: NodeResult -> IO ()
    --      np NodeResult { node,spanned } = putStrLn "HtmlPrinter" >> return () 
    
