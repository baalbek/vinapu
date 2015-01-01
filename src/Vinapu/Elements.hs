{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards  #-}
-- #define RCS_DEBUG

module Vinapu.Elements where

#ifdef RCS_DEBUG
import Control.Monad.Writer (Writer,runWriter,tell,writer)
import Text.Printf (printf)
#endif

import Vinapu.Common (ro2dec)
import Vinapu.LoadSU (LoadSU(..))
import qualified Vinapu.Loads as L
import qualified Vinapu.Nodes as N

data Element = PlateElement {
                n1, n2 :: N.Node,
                wp :: Double,           -- ^ width of plate [m]
                lp :: L.LoadPair,       -- ^ dead and live load pair 
                plw :: Double }         -- ^ Load distribution factor
              | TrapezoidPlateElement {
                n1, n2 :: N.Node,    
                w1 :: Double,           -- ^ width of plate at node n1 [m]
                w2 :: Double,           -- ^ width of plate at node n2 [m]
                lp :: L.LoadPair,       -- ^ dead and live load pair 
                plw :: Double }         -- ^ Load distribution factor
            deriving Show


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

unitLoadAtNode :: N.Node 
                  -> Element 
                  -> Maybe LoadSU
unitLoadAtNode nx el | contains nx el == True = unitLoadAtNode' nx el
                     | otherwise = Nothing

unitLoadAtNode' :: N.Node 
                   -> Element 
                   -> Maybe LoadSU
unitLoadAtNode' _ PlateElement { wp,lp,plw } = 
    let loadFn x = x * wp * plw in Just $ L.loadSU loadFn lp
unitLoadAtNode' node el@TrapezoidPlateElement { .. } 
#ifdef RCS_DEBUG
    = Nothing
#else
    | node == n1 = let loadFn' = loadFn w1 in Just $ L.loadSU loadFn' lp 
    | node == n2 = let loadFn' = loadFn w2 in Just $ L.loadSU loadFn' lp 
    | otherwise = let intpW = interpolatedWidth el node
                      loadFn' = loadFn intpW in Just $ L.loadSU loadFn' lp
        where loadFn wp x = x * wp * plw
#endif
               
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
          
                     
