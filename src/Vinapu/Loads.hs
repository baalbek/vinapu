{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.Loads where

import Text.Printf (printf)
import qualified Data.Map as Map
import Vinapu.LoadSU (LoadSU(..))

type LoadMap = Map.Map Int [DistLoad]

type LoadMap2 = Map.Map Int DistLoad -- LoadPair

type LoadFn = (Double -> Double)

-- newtype LoadType = LoadType { getT :: Int } deriving Show

data LoadType = DEAD_LOAD | LIVE_LOAD deriving (Show,Eq)

data DistLoad = 
    UniformDistLoad {
        oid :: Int,           -- ^ Database primary key
        lt  :: LoadType,      -- ^ Type of load, 1: dead load, 2: live loa-- ^ Type of load, 1: dead load, 2: live loadd 
        desc :: String,       -- ^ Description 
        slsx :: Double,       -- ^ Uniform load pr m2 (servicablity limit) [kN/m2]
        ulsx :: Double        -- ^ Uniform load pr m2 (ultimate limit) [kN/m2]
    } 
    | Snow {
        qm2 :: Double,        -- ^ Uniform load pr m2 (bruksgrenetilstand) [kN/m2]. Automatically adjust brudd with load factor 1.5
        formFactor :: Double, -- ^ form factor (formfaktor etc). Multiplies qm2 
        descx :: String        -- ^ Description 
    } 
    | EmptyLoad deriving Show

data LoadPair = LoadPair {
                    deadLoad :: DistLoad,   -- ^  distributed service load [kN/m2]
                    liveLoad :: DistLoad    -- ^  distributed live load [kN/m2]
                } deriving Show

sumDistLoads :: LoadType -> String -> [DistLoad] -> DistLoad
sumDistLoads loadType loadDesc ([]) = EmptyLoad 
sumDistLoads loadType _ (x:[]) = if (lt x) == loadType 
                                    then 
                                        x 
                                    else
                                        EmptyLoad
sumDistLoads loadType loadDesc loads = UniformDistLoad (-1) loadType loadDesc sls' uls' 
    where sls' = (sum . map sls) loads 
          uls' = (sum . map uls) loads 
    

descLT :: DistLoad -> String
descLT ld = case (lt ld) of 
                DEAD_LOAD -> "Egenlast"
                LIVE_LOAD -> "Nyttelast"

-- Bruksgrensetilstand : Serviceability Limit State (SLS)
-- Bruddgrensetilstand : Ultimate Limit State (ULS)

obliqueLoadSU :: LoadFn      -- ^ Dead load function 
                 -> LoadFn   -- ^ Live load function
                 -> LoadPair  
                 -> LoadSU 
obliqueLoadSU deadLoadFn liveLoadFn LoadPair { deadLoad,liveLoad } = LoadSU sls' uls'
    where sls' = 0.0 -- deadLoadFn $ (sls deadLoad) + (sls liveLoad)
          uls' = 0.0 -- liveLoadFn $ (uls deadLoad) + (uls liveLoad)
          --uls' | liveLoad == Nothing = 0.0
          --     | otherwise = 2.0

-- | Uniform load pr m2 (servicablity limit) [kN/m2]
sls :: DistLoad -> Double
sls UniformDistLoad { slsx } = slsx
sls (EmptyLoad) = 0.0

-- | Uniform load pr m2 (ultimate limit) [kN/m2]
uls :: DistLoad -> Double
uls UniformDistLoad { ulsx } = ulsx
uls (EmptyLoad) = 0.0

loadSU :: LoadFn -> LoadPair -> LoadSU 
loadSU loadFn LoadPair { deadLoad,liveLoad } = LoadSU sls' uls'
    where sls' = loadFn $ (sls deadLoad) + (sls liveLoad) 
          uls' = loadFn $ (uls deadLoad) + (uls liveLoad) 

loadSU1 :: DistLoad -> LoadSU 
loadSU1 ld =  LoadSU (sls ld) (uls ld)


people :: DistLoad 
people = UniformDistLoad 0 LIVE_LOAD "Nyttelast dekke" 2.0 1.6 

concreteSlab :: Double       -- ^ Thickness of slab [mm]
                -> DistLoad 
concreteSlab t = UniformDistLoad 0 DEAD_LOAD (printf "Betong dekke t=%.0fmm" t) (24 * t / 1000.0) 1.2 

ytong :: Double       -- ^ Thickness of slab [mm]
         -> DistLoad
ytong t = UniformDistLoad 0 DEAD_LOAD (printf "Ytong dekke t=%.0fmm" t) (5.5 * t / 1000.0) 1.2 

