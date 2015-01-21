{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.Nodes where

import qualified Data.Map as Map

type NodeId = String

type NodeMap = Map.Map String Node

data Node = Node {
                nodeId :: NodeId, 
                xCoord :: Double, -- ^ x coordinate [m]
                --yCoord :: Double, -- ^ y coordinate [m]
                zCoord :: Double  -- ^ z coordinate [m]. Default 0, != 0 for element m/ vinkel (som er vinkelrett på retning for platevinkler)
            } deriving Show


instance Eq Node where
    (==) n1 n2 = (nodeId n1) == (nodeId n2)
    --(==) n1 n2 = ((xCoord n1) == (xCoord n2)) &&
    --           ((zCoord n1) == (zCoord n2)) 

instance Ord Node where
    compare n1 n2 = compare (xCoord n1) (xCoord n2)

-- | Distance between two nodes along x axis
dist :: Node -> Node -> Double
dist n1 n2 = abs $ (xCoord n2 ) - (xCoord n1)

-- | XY-planets vinkel for to noder i radianer
angle :: Node -> Node -> Double
angle n1 n2 = asin $ (z2 - z1) / len
    where x1 = xCoord n1 
          --y1 = yCoord n1
          z1 = zCoord n1
          x2 = xCoord n2 
          --y2 = yCoord n2
          z2 = zCoord n2
          --len = sqrt $ (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2
          len = sqrt $ (x2-x1)**2 + (z2-z1)**2


