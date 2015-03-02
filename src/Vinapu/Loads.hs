{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.Loads where

import Text.Printf (printf)
import qualified Data.Map as Map
import Vinapu.LoadSU (LoadSU(..))

type LoadMap = Map.Map String DistLoad -- LoadPair

data DistLoad = UniformDistLoad {
                qm2 :: Double,        -- ^ Uniform load pr m2 (bruksgrenetilstand) [kN/m2]
                loadFactor :: Double, -- ^ load factor (bruks/brudd). Multiplies qm2 -> bruddgrensetilstand
                desc :: String        -- ^ Description 
            } 
            | Snow {
                qm2 :: Double,        -- ^ Uniform load pr m2 (bruksgrenetilstand) [kN/m2]. Automatically adjust brudd with load factor 1.5
                formFactor :: Double, -- ^ form factor (formfaktor etc). Multiplies qm2 
                desc :: String        -- ^ Description 
            } deriving Show

data LoadPair = LoadPair {
                    deadLoad :: DistLoad, -- ^  distributed service load [kN/m2]
                    liveLoad :: DistLoad  -- ^  distributed live load [kN/m2]
                } deriving Show

-- Bruksgrensetilstand : Serviceability Limit State (SLS)
-- Bruddgrensetilstand : Ultimate Limit State (ULS)


-- | Kalkulerer last for bruksgrensetilstand 
sls :: DistLoad -> Double
sls (UniformDistLoad {qm2}) = qm2
sls (Snow {qm2,formFactor}) = qm2*formFactor

-- | Kalkulerer last for bruddrensetilstand 
uls :: DistLoad -> Double
uls (UniformDistLoad {qm2,loadFactor}) = qm2*loadFactor
uls (Snow {qm2,formFactor}) = qm2*formFactor*1.5

loadSU :: (Double -> Double) -> LoadPair -> LoadSU 
loadSU loadFn LoadPair { deadLoad,liveLoad } = LoadSU sls' uls'
    where sls' = loadFn $ (sls deadLoad) + (sls liveLoad)
          uls' = loadFn $ (uls deadLoad) + (uls liveLoad)

loadSU1 :: DistLoad -> LoadSU 
loadSU1 ld =  LoadSU (sls ld) (uls ld)

--loadSU1 :: (Double -> Double) -> DistLoad -> LoadSU 
--loadSU1 loadFn ld = LoadSU (loadFn (sls ld)) (loadFn (uls ld))

people :: DistLoad 
people = UniformDistLoad 2.0 1.6 "Nyttelast dekke"

concreteSlab :: Double       -- ^ Thickness of slab [mm]
                -> DistLoad 
concreteSlab t = UniformDistLoad (24 * t / 1000.0) 1.2 (printf "Betong dekke t=%.0fmm" t)

ytong :: Double       -- ^ Thickness of slab [mm]
         -> DistLoad
ytong t = UniformDistLoad (5.5 * t / 1000.0) 1.2 (printf "Ytong dekke t=%.0fmm" t)

--predefLoads :: LoadMap -- Map.Map String LoadPair 
--predefLoads = Map.fromList [
--            ("wood-floor", LoadPair (UniformDistLoad 0.5 1.2 "Wood Floor") people)
--          ]


