{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.Dormers where

import Data.Monoid ((<>))
import Control.Monad.Writer (Writer,runWriter,tell)
import System.Environment (getArgs)
import System.IO (writeFile)
import Text.XML.Light (parseXMLDoc,Element)
import Text.Printf (printf)

import qualified Vinapu.Common as C
import qualified Vinapu.Loads as L

data DormerLoad = DormerLoad {
                    minL :: Double, -- ^ Minimumslast for trekantlaster
                    maxL :: Double  -- ^ Maximumslast for trekantlaster
                    } deriving Show

data Dormer = 
    -- | Ark m/ utvekslingsbjelke 
    GableDormer {
        -- roofAngle :: Double,          -- ^ Hovedtakvinkel [grader]
        commonRafterLen :: Double,    -- ^ Sperrelengde, horisontalt [m] 
        headerLen :: Double,          -- ^ Bredde mellom langssperrer [m]
        headerOffset :: Double,       -- ^ Avstand (horisontalt) fra møne til utvekslingssperre [m]
        valleyRafterOffset :: Double, -- ^ Avstand (horistontalt) fra veggliv til gradsperre treffer sidesperre [m]
        load :: L.LoadPair  
    }
    -- | Ark uten utvekslingsbjelke, 
    -- arkens mønebjelke sammenføyd m/ hovedmønebjelke hus
    | GableDormer2 {
        commonRafterLen :: Double,    -- ^ Sperrelengde, horisontalt [m] 
        headerLen :: Double,          -- ^ Bredde mellom langssperrer [m]
        valleyRafterOffset :: Double, -- ^ Avstand (horistontalt) fra veggliv til gradsperre treffer sidesperre [m]
        load :: L.LoadPair
    } deriving Show

-- | Jevnt fordelt last og maks trekantlast for arkens mønebjelke [kN/m]
ridgeLoad :: Dormer -> Double 
ridgeLoad dorm =  0.5 * w * (L.ultimate lr)
    where lr = L.loadResult load
          w = headerLen dorm

-- | Jevnt fordelt last og maks trekantlast for gradsperre [kN/m]
valleyRafterLoad :: Dormer ->  Double
valleyRafterLoad dorm = jacks1 + jacks2 
    where lr = L.loadResult (load dorm)
          ult = (L.ultimate lr)
          jacks1 = 0.25 * (headerLen dorm) * ult
          jacks2 = 0.5 * (jackLen dorm) * ult

-- | Jevnt fordelt last og maks trekantlast for utvekslingsbjelke [kN/m]
headerLoad :: Dormer -> DormerLoad
headerLoad dorm@GableDormer { .. } = DormerLoad hl (hl + jacks)
    where lr = L.loadResult load 
          ult = (L.ultimate lr)       
          hl = 0.5 * headerOffset * ult 
          jacks = 0.5 * (jackLen dorm) * ult

-- | Maks lengde på sperrer fra utvekslingsbjelke til gradsperre [m]
jackLen :: Dormer -> Double
jackLen GableDormer { commonRafterLen,headerOffset,valleyRafterOffset } = commonRafterLen - headerOffset - valleyRafterOffset 
jackLen GableDormer2 { commonRafterLen,valleyRafterOffset } = commonRafterLen - valleyRafterOffset 


-- | Lengde på arkens mønebjelke [m] 
ridgeLen :: Dormer -> Double
ridgeLen GableDormer { commonRafterLen,headerOffset } = commonRafterLen - headerOffset 
ridgeLen GableDormer2 { commonRafterLen } = commonRafterLen 

-- | Lengde (horisontal) på arkens gradsperre [m] 
valleyRafterLen :: Dormer -> Double 
valleyRafterLen dorm = jlen / (cos ang)
    where ang = atan $ (headerLen dorm) / (2.0 * jlen)
          jlen = jackLen dorm

headerToString :: Dormer -> String
headerToString GableDormer { .. } = undefined

valleyRafterToString :: Dormer -> String
valleyRafterToString dorm = printf "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n" n1 n2 n3 b1 b2 l1 l2 bt m
    where vlen = valleyRafterLen dorm 
          qz1 = valleyRafterLoad dorm
          qz2 = qz1 / 2.0
          n1 = "Node n1 0.0 0.0 0 0 1"
          n2 = printf "Node n2 %.1f 0.0 0 1 1" (vlen/2.0) :: String
          n3 = printf "Node n3 %.1f 0.0 0 0 1" vlen :: String
          b1 = "Beam b1 n1 n2 bt1 l1"
          b2 = "Beam b2 n2 n3 bt1 l2"
          l1 = printf "Load l1 -%.2f -%.2f 0.0 0.0 1.4" qz1 qz2 :: String
          l2 = printf "Load l2 -%.2f 0.0 0.0 0.0 1.4" qz2 :: String
          bt = "BeamType RectangularBeam bt1 140.0 405.0 w1"
          m = "Material Wood w1 C16"
    
createDormer :: Element -> Dormer
createDormer el = GableDormer 7.18 4.854 0.954 2.255 lp 
    where sl = L.Snow 4.5 0.8
          dl = L.UniformDistLoad 0.6 1.2
          lp = L.LoadPair dl sl

toMatstat :: Element -> String -> IO ()
toMatstat el outputDir = do
    --writeFile (outputDir ++ "/header") $ headerToString dormer
    writeFile (outputDir ++ "/valleyrafter") $ valleyRafterToString dormer
    where dormer = createDormer el


main = do
    [fname,outputDir] <- getArgs
    s <- readFile fname 
    case parseXMLDoc s of
        Nothing -> error "Failed to parse xml"
        Just doc  -> toMatstat doc outputDir 
    
