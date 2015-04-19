{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.Loads where

import qualified Data.Map as Map
import Vinapu.LoadSU (LoadSU(..))

type LoadMap = Map.Map Int [DistLoad]

type LoadFn = (Double -> Double)

-- newtype LoadType = LoadType { getT :: Int } deriving Show

data LoadType = DEAD_LOAD | LIVE_LOAD deriving (Show,Eq)

data DistLoad = UniformDistLoad {
                oid :: Int,           -- ^ Database primary key
                lt  :: LoadType,      -- ^ Type of load, 1: dead load, 2: live loa-- ^ Type of load, 1: dead load, 2: live loadd 
                desc :: String,       -- ^ Description 
                sls :: Double,        -- ^ Uniform load pr m2 (servicablity limit) [kN/m2]
                uls :: Double         -- ^ Uniform load pr m2 (ultimate limit) [kN/m2]
            } deriving Show

data LoadPair = LoadPair {
                    deadLoad :: DistLoad,   -- ^  distributed service load [kN/m2]
                    liveLoad :: DistLoad    -- ^  distributed live load [kN/m2]
                } deriving Show

sumDistLoads :: LoadType -> String -> [DistLoad] -> DistLoad
sumDistLoads loadType loadDesc  ([]) = UniformDistLoad (-1) loadType loadDesc 0.0 0.0
sumDistLoads _ _ (x:[]) = x
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


loadSU :: LoadFn -> LoadPair -> LoadSU 
loadSU loadFn LoadPair { deadLoad,liveLoad } = LoadSU sls' uls'
    where sls' = loadFn $ (sls deadLoad) + (sls liveLoad) 
          uls' = loadFn $ (uls deadLoad) + (uls liveLoad) 

{-
    where sls' = case liveLoad of 
                    Nothing -> loadFn $ (sls deadLoad) 
                    Just lv -> loadFn $ (sls deadLoad) + (sls lv) 
          uls' = case liveLoad of 
                    Nothing -> loadFn $ (uls deadLoad) 
                    Just lv -> loadFn $ (uls deadLoad) + (uls lv) 
 -}               

loadSU1 :: DistLoad -> LoadSU 
loadSU1 ld =  LoadSU (sls ld) (uls ld)
