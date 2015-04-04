{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.Loads where

import Text.Printf (printf)
import qualified Data.Map as Map
import Vinapu.LoadSU (LoadSU(..))

type LoadMap = Map.Map Int DistLoad -- LoadPair

type LoadFn = (Double -> Double)

data DistLoad = UniformDistLoad {
                oid :: Int,           -- ^ Database primary key
                desc :: String,       -- ^ Description 
                sls :: Double,        -- ^ Uniform load pr m2 (servicablity limit) [kN/m2]
                uls :: Double         -- ^ Uniform load pr m2 (ultimate limit) [kN/m2]
            } deriving Show

data LoadPair = LoadPair {
                    deadLoad :: DistLoad, -- ^  distributed service load [kN/m2]
                    liveLoad :: DistLoad  -- ^  distributed live load [kN/m2]
                } deriving Show

-- Bruksgrensetilstand : Serviceability Limit State (SLS)
-- Bruddgrensetilstand : Ultimate Limit State (ULS)

obliqueLoadSU :: LoadFn      -- ^ Dead load function 
                 -> LoadFn   -- ^ Live load function
                 -> LoadPair  
                 -> LoadSU 
obliqueLoadSU deadLoadFn liveLoadFn LoadPair { deadLoad,liveLoad } = LoadSU sls' uls'
    where sls' = deadLoadFn $ (sls deadLoad) + (sls liveLoad)
          uls' = liveLoadFn $ (uls deadLoad) + (uls liveLoad)


loadSU :: LoadFn -> LoadPair -> LoadSU 
loadSU loadFn LoadPair { deadLoad,liveLoad } = LoadSU sls' uls'
    where sls' = loadFn $ (sls deadLoad) + (sls liveLoad)
          uls' = loadFn $ (uls deadLoad) + (uls liveLoad)

loadSU1 :: DistLoad -> LoadSU 
loadSU1 ld =  LoadSU (sls ld) (uls ld)

{-
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
-}
