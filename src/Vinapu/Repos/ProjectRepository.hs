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
    fromRow = P.Project <$> field <*> field <*> field

sql :: Query
sql = Query (UTF8.fromString (printf "select p.pn,loc.loc_name,s.sys_name from geometry.projects p join geometry.locations loc on project_id=p.oid join geometry.systems s on s.loc_id=loc.oid where s.oid=?" :: String))

fetchProject :: Connection 
              -> Int           -- ^ System Id
              -> IO P.Project
fetchProject conn sysId = 
    let result = (query conn sql [sysId]) :: IO [P.Project] in
        result >>= \rs ->
        return (head rs)


