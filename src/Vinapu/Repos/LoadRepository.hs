{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Vinapu.Repos.LoadRepository where

import Text.Printf (printf)
import Data.List (nub)
import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (Connection,query,query_)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)

import qualified Vinapu.Loads as L

data LoadDTO = 
    LoadDTO {
        eId :: Int,  -- ^ Element Database Id
        lId :: Int,    -- ^ Load Database Id 
        dsc :: String, -- ^ Load Description 
        lcat :: Int,   -- ^ Load Type (1: dead load, 2: live load)
        sls :: Double, -- ^ Serviablity Limit Load
        uls :: Double  -- ^ Ultimate Limit Load
    } 

instance Show LoadDTO where
    show dto = printf "[eid: %d, lst.id: %d] %s, bruk: %.2f, brudd: %.2f" (eId dto) (lId dto) (dsc dto) (sls dto) (uls dto)

instance FromRow LoadDTO where
    fromRow = LoadDTO <$> field <*> field <*> field <*> field <*> field <*> field

fetchLoads :: Connection
              -> Int           -- ^ System Id
              -> IO [LoadDTO]
fetchLoads conn sysId =
    (query conn "select oid,l_id,dsc,lcat,service_limit,ultimate_limit from construction.v_vinapu_element_loads where sys_id=? order by oid,l_id" [sysId]) :: IO [LoadDTO]

uniqueOids :: [LoadDTO] -> [Int]
uniqueOids = nub . map eId

type OidLoads = (Int,[L.DistLoad])

dtoAsDistLoad :: LoadDTO -> L.DistLoad
dtoAsDistLoad dbl = L.UniformDistLoad (lId dbl) loadType (dsc dbl) (sls dbl) (uls dbl)
    where loadType= case (lcat dbl) of 
                        1 -> L.DEAD_LOAD
                        2 -> L.LIVE_LOAD

loadsForOid :: [LoadDTO]
               -> Int 
               -> OidLoads 
loadsForOid lx oid = (oid,distLoads)
    where lxx = filter (\x -> eId x == oid) lx
          distLoads = map dtoAsDistLoad lxx

loadsAsMap :: Connection 
              -> Int         -- ^ System Id
              -> IO L.LoadMap
loadsAsMap conn sysId = fetchLoads conn sysId >>= \lx ->
                        let uq = uniqueOids lx in
                        return ((Map.fromList . map (loadsForOid lx)) uq)
                            
                               
