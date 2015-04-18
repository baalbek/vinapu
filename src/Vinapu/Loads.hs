{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.Loads where

import qualified Data.Map as Map
import Vinapu.LoadSU (LoadSU(..))

type LoadMap = Map.Map Int [DistLoad]

type LoadFn = (Double -> Double)

newtype LoadType = LoadType { getT :: Int } deriving Show

data DistLoad = UniformDistLoad {
                oid :: Int,           -- ^ Database primary key
                lt  :: LoadType,      -- ^ Type of load, 1: dead load, 2: live loa-- ^ Type of load, 1: dead load, 2: live loadd 
                desc :: String,       -- ^ Description 
                sls :: Double,        -- ^ Uniform load pr m2 (servicablity limit) [kN/m2]
                uls :: Double         -- ^ Uniform load pr m2 (ultimate limit) [kN/m2]
            } deriving Show

data LoadPair = LoadPair {
                    deadLoad :: DistLoad, -- ^  distributed service load [kN/m2]
                    liveLoad :: Maybe DistLoad  -- ^  distributed live load [kN/m2]
                } deriving Show

descLT :: LoadType -> String
descLT (LoadType lt) = case lt of 
                        1 -> "Egenlast"
                        2 -> "Nyttelast"
                        _ -> "N/A"

ltDead :: LoadType
ltDead = LoadType 1

ltLive :: LoadType
ltLive = LoadType 2

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
    where sls' = case liveLoad of 
                    Nothing -> loadFn $ (sls deadLoad) 
                    Just lv -> loadFn $ (sls deadLoad) + (sls lv) 
          uls' = case liveLoad of 
                    Nothing -> loadFn $ (uls deadLoad) 
                    Just lv -> loadFn $ (uls deadLoad) + (uls lv) 
                

loadSU1 :: DistLoad -> LoadSU 
loadSU1 ld =  LoadSU (sls ld) (uls ld)
