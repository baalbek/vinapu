{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}
module Vinapu.ElementResults where

import Text.Printf (printf)

import qualified Vinapu.Nodes as N
import qualified Vinapu.LoadSU as L

data NodeResult = NodeResult {
                node :: N.Node,
                load :: Maybe L.LoadSU
            } deriving Show

data ElementResult = ElementResult {
                nr1, nr2 :: NodeResult
            } deriving Show

loadStr :: Maybe L.LoadSU -> String
loadStr load = case load of Nothing -> "0.0"
                            Just ld -> let sload = (L.service ld)
                                           uload = (L.ultimate ld) in
                                            printf "Service: %.2f, ultimate: %.2f" sload uload

printNodeResult :: NodeResult -> IO ()
printNodeResult NodeResult { node,load } = 
    let nodeStr = printf "\tNode %s: " (N.nodeId node) in 
    putStr nodeStr >>
    putStrLn (loadStr load) >> return ()
    

printElementResult :: ElementResult -> IO ()
printElementResult ElementResult { nr1,nr2 } =
    putStrLn "Element Result:" >>
    printNodeResult nr1 >>
    printNodeResult nr2 >>
    return ()

