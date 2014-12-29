module Vinapu.Common where

import Data.Monoid (Monoid,mempty,mappend)

radians :: Double -> Double
radians d = d * pi / 180.0

degrees :: Double -> Double
degrees r = r * 180.0 / pi

data LimitState = SLS | ULS  deriving (Show,Eq)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }  

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f [] 

instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    mappend (DiffList f) (DiffList g) = DiffList (\xs -> f (g xs)) 

ro2dec :: Double -> Int -> Double
ro2dec v n = (fromInteger $ round $ v * (10^n))/(10^n)

partition' :: Int -> Int -> [a] -> [[a]]
partition' _ _ [] = []
partition' n dropCount xs = (take n xs) : (partition n dropCount (drop dropCount xs))

partition :: Int -> Int -> [a] -> [[a]]
partition n dropCount xs = filter (\x -> (length x) == n) $ result
    where result = partition' n dropCount xs

