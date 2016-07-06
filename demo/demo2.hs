
import qualified Text.XML.Light as X 

import qualified Data.Map as Map
import qualified Vinapu.XML.XmlLoads as XL
import qualified Vinapu.XML.XmlElements as XE
import qualified Vinapu.XML.XmlNodes as XN
import qualified Vinapu.Nodes as N
import Vinapu.Common (partition)

run1 :: X.Element 
        -> IO () -- N.NodeMap
run1 doc = 
    let loads = XL.createVinapuLoads doc 
        lcel = XE.loadCase doc "1"  
        nodes = XN.createVinapuNodes lcel 
        elx = XE.createVinapuElements lcel nodes loads 
        nxp = partition 2 1 (Map.elems nodes)
    in
        putStrLn (show ((head . head) nxp)) >>
        return () 


test1 :: IO () -- N.NodeMap
test1 = 
    readFile "/home/rcs/opt/haskell/vinapu/demo/laster.xml"  >>= \s ->
        case X.parseXMLDoc s of
            Nothing -> error "Failed to parse xml"
            Just doc -> run1 doc --putStrLn (show doc) --S.runVinapuXml doc lc printers
        >> return ()





