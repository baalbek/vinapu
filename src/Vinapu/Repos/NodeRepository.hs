{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Vinapu.Repos.NodeRepository where

import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (Connection,query)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)

import qualified Vinapu.Nodes as N

instance FromRow N.Node where
    fromRow = N.Node <$> field <*> field <*> field <*> field <*> field

fetchNodes :: Connection 
              -> Int           -- ^ System Id
              -> IO [N.Node]
fetchNodes conn sysId = 
    (query conn "select n.oid,n.dsc,n.x,n.y,n.z from construction.nodes n join construction.vinapu_elements e on n.oid=e.n1 or n.oid=e.n2 where e.sys_id=?" [sysId]) :: IO [N.Node]

fetchNodesAsMap :: Connection 
                   -> Int  -- ^ System Id
                   -> IO N.NodeMap
fetchNodesAsMap conn sysId = fetchNodes conn sysId >>= \nodes ->
    return (Map.fromList (map asListItem nodes))
        where asListItem x = (N.oid x, x)
    
