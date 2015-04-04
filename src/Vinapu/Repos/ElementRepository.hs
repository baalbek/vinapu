{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Vinapu.Repos.ElementRepository where

import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (Connection,query,query_)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)

import qualified Vinapu.Loads as L
import qualified Vinapu.Nodes as N
import qualified Vinapu.Elements as E

data ElementDTO = 
    ElementDTO {
        oid :: Int,
        desc :: String,
        n1 :: Int,
        n2 :: Int,
        slsSingle :: Maybe Int,
        slsComposite :: Maybe Int,
        ulsSingle :: Maybe Int,
        ulsComposite :: Maybe Int,
        plw :: Double,
        w1 :: Double,
        w2 :: Maybe Double,
        angle :: Double,
        elementType :: Int
    } deriving Show

instance FromRow ElementDTO where
    fromRow = ElementDTO <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

fetchElementDTOs :: Connection
                 -> Int      -- ^ System id
                 -> IO [ElementDTO]
fetchElementDTOs conn sysId =  
    (query conn "select oid,dsc,n1,n2,sls_single,sls_composite,uls_single,uls_composite,plw,w1,w2,angle,element_type from construction.vinapu_elements where sys_id=?" [sysId]) :: IO [ElementDTO]


createElement :: N.NodeMap -- ^ Nodes
                 -> L.LoadMap -- ^ Single loads 
                 -> L.LoadMap -- ^ Composite loads 
                 -> ElementDTO
                 -> E.Element
createElement nm slm clm dto =
    E.PlateElement (oid dto) (desc dto) n1 n2 (plw dto) (w1 dto)
        where Just n1 = Map.lookup (n1 dto) nm
              Just n2 = Map.lookup (n2 dto) nm

fetchElements :: Connection
                 -> Int      -- ^ System id
                 -> N.NodeMap -- ^ Nodes
                 -> L.LoadMap -- ^ Single loads 
                 -> L.LoadMap -- ^ Composite loads 
                 -> IO [E.Element]
fetchElements conn sysId nm slm clm =
    fetchElementDTOs conn sysId >>= \dtos ->
    let createElement' = createElement nm slm clm in
    return (map createElement' dtos)
    -- return []



