{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Vinapu.Repos.LoadRepository where

import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (Connection,query,query_)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)

import qualified Vinapu.Loads as L


instance FromRow L.DistLoad where
    fromRow = L.UniformDistLoad <$> field <*> field <*> field <*> field 

fetchSingleLoads :: Connection 
                  -> Int             -- ^ System id
                  -> IO [L.DistLoad]
fetchSingleLoads conn sysId = 
    (query conn "select l.* from construction.v_single_loads l join construction.vinapu_elements e on ((e.permanent_single=l.oid) or (e.live_single=l.oid)) where e.sys_id=?" [sysId]) :: IO [L.DistLoad]

fetchCompositeLoads :: Connection 
                       -> Int             -- ^ System id
                       -> IO [L.DistLoad]
fetchCompositeLoads conn sysId = 
    (query conn "select l.* from construction.v_composite_loads l join construction.vinapu_elements e on ((e.permanent_composite=l.oid) or (e.live_composite=l.oid)) where e.sys_id=?" [sysId]) :: IO [L.DistLoad]


singleLoadsAsMap :: Connection
                    -> Int             -- ^ System id
                    -> IO L.LoadMap
singleLoadsAsMap conn sysId = 
    fetchSingleLoads conn sysId >>= \loads ->
    return (Map.fromList (map asListItem loads))
        where asListItem x = (L.oid x, x)

compositeLoadsAsMap :: Connection
                    -> Int             -- ^ System id
                    -> IO L.LoadMap
compositeLoadsAsMap conn sysId = 
    fetchCompositeLoads conn sysId >>= \loads ->
    return (Map.fromList (map asListItem loads))
        where asListItem x = (L.oid x, x)
