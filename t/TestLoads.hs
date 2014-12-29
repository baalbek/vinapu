{-# LANGUAGE OverloadedStrings #-}

module TestLoads where 

import Test.HUnit (test,assertEqual,(~:))

import qualified Vinapu.LoadSU as SU
import qualified Vinapu.Loads as L
import Vinapu.Common (ro2dec)

testLoads = test [
    "L1" ~: do 
        let conc = L.concreteSlab 200
        let snow = L.Snow 4.5 0.8
        let lp = L.LoadPair conc snow
        assertEqual "[Conc sls]" 4.8 (ro2dec (L.sls conc) 1)
        assertEqual "[Conc uls]" 5.8 (ro2dec (L.uls conc) 1)
        assertEqual "[Snow sls]" 3.6 (ro2dec (L.sls snow) 1)
        assertEqual "[Snow uls]" 5.4 (ro2dec (L.uls snow) 1)
        let loadResult = L.loadSU1 lp  
        assertEqual "[Result service]" 8.4 (ro2dec (SU.service loadResult) 1)
        assertEqual "[Result ultimate]" 11.2 (ro2dec (SU.ultimate loadResult) 1)
    ]
