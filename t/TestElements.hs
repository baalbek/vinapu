{-# LANGUAGE OverloadedStrings #-}

module TestElements where 

import Test.HUnit (test,assertEqual,(~:))

import qualified Vinapu.Nodes as N
import Vinapu.LoadSU ((<+>),(<++>),LoadSU(..))
import Vinapu.Loads (concreteSlab,DistLoad(..),LoadPair(..))
import qualified Vinapu.Elements as E
import Vinapu.Common (LimitState,ro2dec)

n1 = N.Node 1 0 0 
n2 = N.Node 2 3 0 
n3 = N.Node 3 5 0 
n4 = N.Node 4 10 0 
n5 = N.Node 5 15 0 

nodes = [n1,n2,n3,n4]

conc = concreteSlab 200
snow = Snow 4.5 0.8
lp = LoadPair conc snow

testElements = test [
    "E1" ~: do 
        let e1 = E.PlateElement n2 n4 5.0 lp 0.5
        assertEqual "[Contains n1]" False (E.contains n1 e1)
        assertEqual "[Contains n2]" True (E.contains n2 e1)
        assertEqual "[Contains n3]" True (E.contains n3 e1)
        assertEqual "[Contains n4]" True (E.contains n4 e1)
        assertEqual "[Contains n5]" False (E.contains n5 e1)
        assertEqual "[LoadAtNode n1]" Nothing (E.loadAtNode n1 e1)
        assertEqual "[LoadAtNode n2]" (Just $ LoadSU 21.0 27.9) (E.loadAtNode n2 e1)
        assertEqual "[LoadAtNode n3]" (Just $ LoadSU 21.0 27.9) (E.loadAtNode n3 e1)
        assertEqual "[LoadAtNode n4]" (Just $ LoadSU 21.0 27.9) (E.loadAtNode n4 e1)
        assertEqual "[LoadAtNode n5]" Nothing (E.loadAtNode n5 e1),
    "E2" ~: do 
        let e1 = E.PlateElement n1 n2 2.0 lp 0.5
        let e2 = E.PlateElement n2 n4 5.0 lp 0.5
        let e3 = E.PlateElement n4 n5 5.0 lp 0.63
        assertEqual "ssdf" 1 1
    ]


