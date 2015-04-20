{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards  #-}
-- #define RCS_DEBUG

module Vinapu.Elements where

#ifdef RCS_DEBUG
import Control.Monad.Writer (Writer,runWriter,tell,writer)
#endif

import Text.Printf (printf)
import Vinapu.Common (radians)
import Vinapu.LoadSU (LoadSU(..))
import qualified Vinapu.Loads as L
import qualified Vinapu.Nodes as N


data Element = 
    -- | Database element type 1
    PlateElement {
                oid :: Int,                 -- ^ Database primary key
                desc :: String,             -- ^ Descriptiong
                n1, n2 :: N.Node,
                --lp :: L.LoadPair,         -- ^ dead and live load pair 
                lts :: [L.DistLoad],  -- ^ list of loads (both dead/live)
                plw :: Double,              -- ^ Load distribution factor
                ------------------- Unique property combo for this type -------------------
                wp :: Double           -- ^ width of plate [m]
    }         
    -- | Database element type 2
    | ObliquePlateElement {
                oid :: Int,           -- ^ Database primary key
                desc :: String,          -- ^ Descriptiong
                n1, n2 :: N.Node,
                --lp :: L.LoadPair,       -- ^ dead and live load pair 
                plw :: Double,          -- ^ Load distribution factor
                ------------------- Unique property combo for this type -------------------
                wp :: Double,           -- ^ width of plate [m]
                angle :: Double        -- ^ Angle of plate in degrees 
    }         
    -- | Database element type 3
    | TrapezoidPlateElement {
                oid :: Int,           -- ^ Database primary key
                desc :: String,          -- ^ Descriptiong
                n1, n2 :: N.Node,    
                --lp :: L.LoadPair,       -- ^ dead and live load pair 
                plw :: Double,          -- ^ Load distribution factor
                ------------------- Unique property combo for this type -------------------
                w1 :: Double,           -- ^ width of plate at node n1 [m]
                w2 :: Double           -- ^ width of plate at node n2 [m]
    }         
    -- | Database element type 4
    | ObliqueTrapezoidPlateElement {
                oid :: Int,           -- ^ Database primary key
                desc :: String,          -- ^ Descriptiong
                n1, n2 :: N.Node,    
                --lp :: L.LoadPair,       -- ^ dead and live load pair 
                plw :: Double,          -- ^ Load distribution factor
                ------------------- Unique property combo for this type -------------------
                w1 :: Double,           -- ^ width of plate at node n1 [m]
                w2 :: Double,           -- ^ width of plate at node n2 [m]
                angle :: Double        -- ^ Angle of plate in degrees 
    }         
    deriving Show

lp :: Element -> L.LoadPair
lp el = L.LoadPair deadD liveD
    where loads = lts el
          deadLoads = filter (\x -> (L.lt x) == L.DEAD_LOAD) loads
          liveLoads = filter (\x -> (L.lt x) == L.LIVE_LOAD) loads
          deadD = (L.sumDistLoads L.DEAD_LOAD "Sum egenlast" deadLoads) 
          liveD = (L.sumDistLoads L.LIVE_LOAD "Sum nyttelast" liveLoads) 


-- lp el = L.LoadPair (L.UniformDistLoad 1 L.DEAD_LOAD "Test" 2 3 ) Nothing

fullDesc :: Element -> String
fullDesc el = printf "[eid %d] %s (Bredde: %.2f m)" (oid el) (desc el) (wp el)

-- | Checks if nodes na and nb spans element el
spans :: N.Node -> N.Node -> Element -> Bool
spans na nb el | na < nb = not $ n2' <= na || n1' >= nb
               | otherwise = not $ n2' <= nb || n1' >= na 
    where n1' = n1 el
          n2' = n2 el


-- | Checks if node nx is contained in element el.
-- Returns True also if nx == n1 or nx == n2 of element
contains :: N.Node -- ^ Node to check if is contained in element el
            -> Element 
            -> Bool
contains nx el = not $ nx < n1' || nx > n2'
    where n1' = n1 el
          n2' = n2 el

loadPairAtNode :: N.Node 
                  -> Element 
                  -> Maybe L.LoadPair
loadPairAtNode nx el | contains nx el == True = Just (lp el)
                     | otherwise = Nothing

unitLoadAtNode :: N.Node 
                  -> Element 
                  -> Maybe LoadSU
unitLoadAtNode nx el | contains nx el == True = unitLoadAtNode' nx el
                     | otherwise = Nothing

unitLoadAtNode' :: N.Node 
                   -> Element 
                   -> Maybe LoadSU
unitLoadAtNode' _ el@PlateElement { wp,plw } = 
    let loadFn x = x * wp * plw in Just $ L.loadSU loadFn (lp el)
unitLoadAtNode' _ el@ObliquePlateElement { angle,wp,plw } = 
    let deadLoadFn x = x * wp * plw / (cos (radians angle))
        liveLoadFn x = x * wp * plw in Just $ L.obliqueLoadSU deadLoadFn liveLoadFn (lp el)
unitLoadAtNode' node el@TrapezoidPlateElement { .. } 
    | node == n1 = let loadFn' = loadFn w1 in Just $ L.loadSU loadFn' lp'
    | node == n2 = let loadFn' = loadFn w2 in Just $ L.loadSU loadFn' lp'
    | otherwise = let intpW = interpolatedWidth el node
                      loadFn' = loadFn intpW in Just $ L.loadSU loadFn' lp'
        where loadFn wp x = x * wp * plw
              lp' = lp el
               
#ifdef RCS_DEBUG
interpolatedWidth :: Element -> N.Node -> Writer String Double
#else
interpolatedWidth :: Element -> N.Node -> Double
#endif
interpolatedWidth TrapezoidPlateElement { n1,n2,w1,w2 } n = 
    let totalDist = N.dist n1 n2
        distMidNode = N.dist n1 n 
        diffW = w2 - w1 
#ifdef RCS_DEBUG
        msg = printf "totalDist: %.2f, distMidNode: %.2f, diffW: %.2f w1: %.2f" totalDist distMidNode diffW w1 :: String
    in writer (w1 + (diffW * distMidNode / totalDist),msg)
#else
    in w1 + (diffW * distMidNode / totalDist)
#endif

                     
