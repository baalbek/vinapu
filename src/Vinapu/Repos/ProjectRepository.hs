{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Vinapu.Repos.ProjectRepository where

import qualified Data.ByteString.UTF8 as UTF8 
import Text.Printf (printf)
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (Connection,query)
import Database.PostgreSQL.Simple.Types (Query(..))
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)

import qualified Vinapu.Projects as P

instance FromRow P.Project where
    fromRow = P.Project <$> field <*> field <*> field <*> field <*> field <*> field


sql :: Query
sql = Query (UTF8.fromString (printf "select p.oid,p.pn,loc.oid,loc.loc_name,s.oid,s.sys_name from geometry.projects p join geometry.locations loc on project_id=p.oid join geometry.systems s on s.loc_id=loc.oid where s.oid=?" :: String))

fetchProject :: Connection 
              -> Int           -- ^ System Id
              -> IO P.Project
fetchProject conn sysId = 
    let result = (query conn sql [sysId]) :: IO [P.Project] in
        result >>= \rs ->
        return (head rs)


instance FromRow P.GeoSystem where
    fromRow = P.GeoSystem <$> field 

fetchGeoSystems :: Connection
               -> Int         -- ^ Project Id 
               -> IO [P.GeoSystem]
fetchGeoSystems conn projId = (query conn q [projId]) :: IO [P.GeoSystem]
    where q = Query (UTF8.fromString (printf "select s.oid from geometry.systems s join geometry.locations l on s.loc_id=l.oid where l.project_id=?" :: String))
    
