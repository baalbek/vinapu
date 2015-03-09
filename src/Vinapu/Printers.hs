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

print :: [ElementResult] -> Printer -> IO ()
print elx (StdoutPrinter) = mapM_ printElementResult elx >> return ()
print elx HtmlPrinter { fileName } = undefined
    where prefix = "<!DOCTYPE html>\n<html>\n<head>\n<title>Tikal!</title>\n</head>\n<body>\n"
          postfix = "\n</body></html>"
    
    -- mapM_ (printElementResult np) elx >> return ()
    --where np :: NodeResult -> IO ()
    --      np NodeResult { node,spanned } = putStrLn "HtmlPrinter" >> return () 
    
