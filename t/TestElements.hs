{-# LANGUAGE OverloadedStrings #-}

module TestElements where 

import Test.HUnit (test,assertEqual,(~:))

import qualified Vinapu.Nodes as N
import Vinapu.LoadSU ((<++>),LoadSU(..))
import Vinapu.Loads (loadSU1,concreteSlab,DistLoad(..),LoadPair(..))
import qualified Vinapu.Elements as E
import Vinapu.Common (LimitState,ro2dec)

n1 = N.Node "n1" 0 0 
n2 = N.Node "n2" 3 0 
n3 = N.Node "n3" 5 0 
n4 = N.Node "n4" 10 0 
n5 = N.Node "n5" 15 0 

nodes = [n1,n2,n3,n4,n5]

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
        assertEqual "[LoadAtNode n1]" Nothing (E.unitLoadAtNode n1 e1)
        assertEqual "[LoadAtNode n2]" (Just $ LoadSU 21.0 27.9) (E.unitLoadAtNode n2 e1)
        assertEqual "[LoadAtNode n3]" (Just $ LoadSU 21.0 27.9) (E.unitLoadAtNode n3 e1)
        assertEqual "[LoadAtNode n4]" (Just $ LoadSU 21.0 27.9) (E.unitLoadAtNode n4 e1)
        assertEqual "[LoadAtNode n5]" Nothing (E.unitLoadAtNode n5 e1),
    "E2" ~: do 
        let e1 = E.PlateElement n1 n2 2.0 lp 0.5
        let e2 = E.PlateElement n2 n4 5.0 lp 0.5
        let e3 = E.PlateElement n4 n5 5.0 lp 0.5
        let e4 = E.PlateElement n3 n5 6.0 lp 0.5
        let elx = [e1,e2,e3,e4]
        let sumNode n = let latn = E.unitLoadAtNode n in foldr (<++>) Nothing $ map latn elx  
        let sumN1 = sumNode n1 
        assertEqual "[Sum LoadAtNode n1]" (Just $ LoadSU 8.4 11.16) sumN1
        let sumN2 = sumNode n2 
        assertEqual "[Sum LoadAtNode n2]" (Just $ LoadSU 29.4 39.06) sumN2
        let sumN3 = sumNode n3 
        let all = map sumNode nodes
        assertEqual "[Sum LoadAtNode n3]" (Just $ LoadSU 46.2 61.38) sumN3,
    "E3" ~: do 
        let e1 = E.TrapeziodPlateElement n1 n3 0.0 5.0 lp 0.5
        assertEqual "[LoadAtNode n1]" (Just $ LoadSU 0.0 0.0) (E.unitLoadAtNode n1 e1)
        assertEqual "[LoadAtNode n3]" (Just $ LoadSU 21.0 27.9) (E.unitLoadAtNode n3 e1)
        assertEqual "[LoadAtNode n2]" (Just $ LoadSU 12.6 16.74) (E.unitLoadAtNode n2 e1),
    "E4" ~: do 
        let e1 = E.TrapeziodPlateElement n1 n3 5.0 0.0 lp 0.5
        assertEqual "[LoadAtNode n1]" (Just $ LoadSU 0.0 0.0) (E.unitLoadAtNode n3 e1)
        assertEqual "[LoadAtNode n3]" (Just $ LoadSU 21.0 27.9) (E.unitLoadAtNode n1 e1)
        assertEqual "[LoadAtNode n2]" (Just $ LoadSU 8.4 11.16) (E.unitLoadAtNode n2 e1)
    ]


