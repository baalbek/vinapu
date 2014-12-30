{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}
module Vinapu.Elements where

import Vinapu.Common (ro2dec)
import Vinapu.LoadSU (LoadSU(..))
import qualified Vinapu.Loads as L
import qualified Vinapu.Nodes as N

data Element = PlateElement {
                n1, n2 :: N.Node,
                wp :: Double,      -- ^ width of plate [m]
                lp :: L.LoadPair,  -- ^ dead and live load pair 
                plw :: Double      -- ^ Lastfordelingsfaktor 
            } deriving Show


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
unitLoadAtNode' _ PlateElement { wp,lp,plw } = Just $ L.loadSU loadFn lp
    where loadFn x = x * wp * plw

{-
createElement :: P.Plate -> N.Node -> N.Node -> Double -> Element
createElement p na nb w | na < nb = PlateElement na nb p w
                        | otherwise = PlateElement nb na p w

serviceLoad :: Element -> Double
serviceLoad PlateElement { plate,plw } = P.load plate C.SLS plw

ultimateLoad :: Element -> Double
ultimateLoad PlateElement { plate,plw } = P.load plate C.ULS plw

contains :: Element -> N.Node -> N.Node -> Bool
contains el na nb | na < nb = not $ n2' <= na || n1' >= nb
                  | otherwise = not $ n2' <= nb || n1' >= na 
    where n1' = n1 el
          n2' = n2 el


loadIf :: (Element -> Double) -> N.Node -> N.Node -> Element -> Double
loadIf loadFn na nb el | contains el na nb = loadFn el
                       | otherwise = 0.0

loadsIf :: (Element -> Double) -> [Element] -> N.Node -> N.Node -> Double
loadsIf loadFn elx na nb = sum $ map loadIf' elx
    where loadIf' = loadIf loadFn na nb

serviceLoadsIf :: [Element] -> N.Node -> N.Node -> Double
serviceLoadsIf = loadsIf serviceLoad

ultimateLoadsIf :: [Element] -> N.Node -> N.Node -> Double
ultimateLoadsIf = loadsIf ultimateLoad

partition' :: Int -> Int -> [a] -> [[a]]
partition' _ _ [] = []
partition' n dropCount xs = (take n xs) : (partition n dropCount (drop dropCount xs))

partition :: Int -> Int -> [a] -> [[a]]
partition n dropCount xs = filter (\x -> (length x) == n) $ result
    where result = partition' n dropCount xs
-}

