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
        oid :: Int,
        desc :: String,
        n1 :: Int,
        n2 :: Int,
        deadSingle :: Maybe Int,
        deadComposite :: Maybe Int,
        liveSingle :: Maybe Int,
        liveComposite :: Maybe Int,
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
    (query conn "select oid,dsc,n1,n2,permanent_single,permanent_composite,live_single,live_composite,plw,w1,w2,angle,element_type from construction.vinapu_elements where sys_id=?" [sysId]) :: IO [ElementDTO]


createElement :: N.NodeMap -- ^ Nodes
                 -> L.LoadMap -- ^ Loads pr element oid 
                 -> ElementDTO
                 -> E.Element
createElement nm lm dto =
    E.PlateElement oid' (desc dto) n1' n2' lts (plw dto) (w1 dto)
        where Just n1' = Map.lookup (n1 dto) nm
              Just n2' = Map.lookup (n2 dto) nm
              oid' = oid dto
              Just lts = mplus (Map.lookup oid' lm) (Just [])
             

{-
createElement :: N.NodeMap -- ^ Nodes
                 -> L.LoadMap -- ^ Single loads 
                 -> L.LoadMap -- ^ Composite loads 
                 -> ElementDTO
                 -> E.Element
createElement nm slm clm dto =
    E.PlateElement (oid dto) (desc dto) n1' n2' (L.LoadPair deadLoad liveLoad) (plw dto) (w1 dto)
        where Just n1' = Map.lookup (n1 dto) nm
              Just n2' = Map.lookup (n2 dto) nm
              Just deadLoad = case (deadSingle dto) of  
                                Just deadLoadId -> Map.lookup deadLoadId slm 
                                Nothing -> deadComposite dto >>= flip Map.lookup clm 
              liveLoad = case (liveSingle dto) of  
                            Just liveLoadId -> Map.lookup liveLoadId slm 
                            Nothing -> case (liveComposite dto) of 
                                Just liveCompositeId -> Map.lookup liveCompositeId clm 
                                Nothing -> Nothing
-}

              

fetchElements :: Connection
                 -> Int      -- ^ System id
                 -> N.NodeMap -- ^ Nodes
                 -> L.LoadMap -- ^ Loads pr element oid 
                 -> IO [E.Element]
fetchElements conn sysId nm lm =
    fetchElementDTOs conn sysId >>= \dtos ->
    let createElement' = createElement nm lm in
    return (map createElement' dtos)



