
import Vinapu.Loads (people,concreteSlab,ytong,DistLoad(..),LoadPair(..))
import qualified Vinapu.Nodes as N
import qualified Vinapu.Elements as E
import qualified Vinapu.System as S
import qualified Vinapu.Loads as L
import qualified Vinapu.LoadSU as LU
import qualified Vinapu.Tables as T
import qualified Vinapu.XML.XmlElements as XE
import qualified Vinapu.XML.XmlNodes as XN
import qualified Vinapu.XML.Common as XC
import qualified Text.XML.Light as X 
import Vinapu.Common (radians)

n1 = N.Node "1" 0 0 "n1"
n2 = N.Node "2" 3 0 "n2" 
n3 = N.Node "3" 4 0 "n3" 
n4 = N.Node "4" 5 0 "n4"
n5 = N.Node "5" 6 0 "n5" 
n6 = N.Node "6" 7 0 "n6"

yt = ytong 300 
snow = Snow 4.5 0.8 "Snow"
lp = LoadPair yt snow
lp2 = LoadPair (concreteSlab 200) snow
lp3 = LoadPair (concreteSlab 300) people

e1 = E.PlateElement n1 n2 (3.9+1.5) lp 0.5 "Akse A1-B3"
e1b = E.PlateElement n1 n3 3.9 lp3 0.5 "Akse A2-C4"
e3 = E.PlateElement n2 n3 (3.6+3.6) lp2 0.5 "Akse D3-D4"
--- e2 = E.PlateElement n3 n4 (3.6+2.1) lp2 0.5
-- e2b = E.PlateElement n3 n4 3.6 lp 0.5
-- e4 = E.PlateElement n5 n6 3 lp 0.5

r4 = S.runVinapu [e1,e1b,e3] [n1,n2,n3]


oe1 = E.ObliquePlateElement 45 n1 n2 10 lp 0.5 "Oblique Akse A1-B3"
xe1 = E.PlateElement n1 n2 10 lp 0.5 "Akse A1-B3"
un1 = E.unitLoadAtNode n1 oe1 
ux1 = E.unitLoadAtNode n1 xe1 

table = T.Table "Table 1" [T.Row "Hus A" "12.1" "33.4",T.Row "Hus B" "34.4" "45"]
r1 = T.load2Row "HI" (Just (LU.LoadSU 12 14))

a1 = XC.createAttr "A" "1"
a2 = XC.createAttr "A" "2"

testme :: IO ()
testme = do
    s <- readFile "/home/rcs/opt/haskell/vinapu/demo/demo.xml"
    case X.parseXMLDoc s of
        Nothing -> error "Failed to parse xml"
        Just doc -> let Just top = XC.xmlElement "load-cases" doc 
                        result = XC.findElementForAttr (XC.xmlElements "lc" top) "id" "default" in 
            putStrLn (show result) >> return () 
    return ()

testme2 :: IO ()
testme2 = do
    s <- readFile "/home/rcs/opt/haskell/vinapu/demo/demo.xml"
    case X.parseXMLDoc s of
        Nothing -> error "Failed to parse xml"
        Just doc -> let lc = XE.loadCase doc "default" 
                        nodes = XN.createVinapuNodes lc in 
                            putStrLn (show nodes) >> return ()
    return ()
