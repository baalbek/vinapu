{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.Nodes where

import qualified Data.Map as Map

type NodeMap = Map.Map Int Node

data Node = Node {
                oid :: Int,           -- ^ Database primary key
                desc :: Maybe String,       -- ^ Description 
                xCoord :: Double, -- ^ x coordinate [m]
                yCoord :: Double, -- ^ y coordinate [m]
                zCoord :: Double  -- ^ z coordinate [m]. Default 0, != 0 for element m/ vinkel (som er vinkelrett på retning for platevinkler)
            } deriving Show


instance Eq Node where
    (==) n1 n2 = (oid n1) == (oid n2)
    --(==) n1 n2 = ((xCoord n1) == (xCoord n2)) &&
    --           ((zCoord n1) == (zCoord n2)) 

instance Ord Node where
    compare n1 n2 | xcmp /= EQ = xcmp
                  | otherwise = ycmp
        where xcmp = compare (xCoord n1) (xCoord n2)
              ycmp = compare (yCoord n1) (yCoord n2)

-- | Distance between two nodes 
dist :: Node -> Node -> Double
dist n1 n2 = sqrt (deltaX**2 + deltaY**2) 
    where deltaX = (xCoord n2 ) - (xCoord n1)
          deltaY = (yCoord n2 ) - (yCoord n1)

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


