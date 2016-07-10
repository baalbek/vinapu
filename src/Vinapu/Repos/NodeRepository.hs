{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE OverloadedStrings #-}


module Vinapu.Repos.NodeRepository where

import Text.Printf (printf)
import qualified Data.ByteString.UTF8 as UTF8 
import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (Connection,query)
import Database.PostgreSQL.Simple.Types (Query(..))
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)

import qualified Vinapu.Nodes as N

instance FromRow N.Node where
    fromRow = N.Node <$> field <*> field <*> field <*> field <*> field

sql :: Query
sql = Query (UTF8.fromString (printf "%s union %s order by 3,4" s1 s2 :: String))
    where s = "select n.oid,n.dsc,n.x,n.y,n.z from geometry.nodes n join geometry.locations l on l.oid=n.loc_id join geometry.systems s on s.loc_id=l.oid join vinapu.elements v on v.%s = n.oid where s.oid=?"
          s1 = printf s "n1" :: String
          s2 = printf s "n2" :: String

fetchNodes :: Connection 
              -> Int           -- ^ System Id
              -> IO [N.Node]
fetchNodes conn sysId = 
    putStrLn (show sql) >>
    (query conn sql [sysId,sysId]) :: IO [N.Node]

fetchNodesAsMap :: Connection 
                   -> Int  -- ^ System Id
                   -> IO N.NodeMap
fetchNodesAsMap conn sysId = fetchNodes conn sysId >>= \nodes ->
    return (nodesAsMap nodes)

nodesAsMap :: [N.Node] 
              -> N.NodeMap
nodesAsMap nodes =
    Map.fromList (map asListItem nodes)
        where asListItem x = (N.oid x, x)
    
