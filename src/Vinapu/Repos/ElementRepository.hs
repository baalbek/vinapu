{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Vinapu.Repos.ElementRepository where

import Control.Monad (mplus)
import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (Connection,query)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)

import qualified Vinapu.Loads as L
import qualified Vinapu.Nodes as N
import qualified Vinapu.Elements as E

data ElementDTO =
    ElementDTO {
        oid :: Int
        ,desc :: String
        ,n1 :: Int
        ,n2 :: Int
        ,plw :: Double
        ,w1 :: Double
        ,w2 :: Maybe Double
        ,angle :: Double
        ,elementType :: Int
        -- wnode :: Maybe Int -- ^ node id to calculate the widht of the plate, ie widht = (N.dist n1 wnode)
    } deriving Show

instance FromRow ElementDTO where
    fromRow = ElementDTO <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

fetchElementDTOs :: Connection
                 -> Int      -- ^ System id
                 -> IO [ElementDTO]
fetchElementDTOs conn sysId =
    (query conn "select oid,dsc,n1,n2,plw,w1,w2,angle,element_type from vinapu.elements where sys_id=?" [sysId]) :: IO [ElementDTO]


createElement :: N.NodeMap -- ^ Nodes
                 -> L.LoadMap -- ^ Loads pr element oid
                 -> ElementDTO
                 -> E.Element
createElement nm lm dto =
    case (elementType dto) of
      1 ->
        E.PlateElement oid' (desc dto) n1' n2' lts (plw dto) (w1 dto)
            where Just n1' = Map.lookup (n1 dto) nm
                  Just n2' = Map.lookup (n2 dto) nm
                  oid' = oid dto
                  Just lts = mplus (Map.lookup oid' lm) (Just [])
      3 ->
        E.TrapezoidPlateElement oid' (desc dto) n1' n2' lts (plw dto) (w1 dto) w2'
            where Just n1' = Map.lookup (n1 dto) nm
                  Just n2' = Map.lookup (n2 dto) nm
                  oid' = oid dto
                  Just w2' = (w2 dto)
                  Just lts = mplus (Map.lookup oid' lm) (Just [])


fetchElements :: Connection
                 -> Int      -- ^ System id
                 -> N.NodeMap -- ^ Nodes
                 -> L.LoadMap -- ^ Loads pr element oid
                 -> IO [E.Element]
fetchElements conn sysId nm lm =
    fetchElementDTOs conn sysId >>= \dtos ->
    let createElement' = createElement nm lm in
    return (map createElement' dtos)
