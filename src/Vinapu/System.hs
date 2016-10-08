
module Vinapu.System where

import qualified Text.XML.Light as X 
import qualified Data.Map as Map

import Database.PostgreSQL.Simple (close)

import qualified Vinapu.Repos.ElementRepository as ER
import qualified Vinapu.Repos.LoadRepository as LR
import qualified Vinapu.Repos.NodeRepository as NR
import qualified Vinapu.Repos.ProjectRepository as PR
import qualified Vinapu.Projects as PJ
import qualified Vinapu.Elements as E
import qualified Vinapu.Nodes as N
import qualified Vinapu.ElementResults as R
import qualified Vinapu.Printers as P
import qualified Vinapu.XML.XmlNodes as XN
import qualified Vinapu.XML.XmlLoads as XL
import qualified Vinapu.XML.XmlElements as XE
import Vinapu.Common (partition,getConnection)

type NodeSpan = [N.Node]


collectSpan :: Maybe PJ.Project -> [E.Element] -> NodeSpan -> R.ElementResult
collectSpan proj elements nodeSpan = R.ElementResult nra nrb proj
    where [na,nb] = nodeSpan 
          spans = E.spans na nb
          spanned = filter spans elements
          nra = R.NodeResult na spanned
          nrb = R.NodeResult nb spanned

collectResults :: [E.Element] ->  [NodeSpan] -> Maybe PJ.Project -> [R.ElementResult]
collectResults elements nodeSpans proj = 
    let collectSpan' = collectSpan proj elements 
    in  map collectSpan' nodeSpans


runVinapu :: [E.Element]
             -> [N.Node]
             -> [P.Printer]
             -> Maybe PJ.Project
             -> IO ()
runVinapu elements nodes printers proj = 
    let nxp = partition 2 1 nodes 
        results = collectResults elements nxp proj in
    mapM_ (P.print results) printers >>
    return ()

runVinapuPostgres :: String    -- ^ Database Host  
                     -> String -- ^ Database Name
                     -> String -- ^ Database User 
                     -> String -- ^ Database password 
                     -> Int    -- ^ System Id
                     -- -> Int    -- ^ Load Case
                     -> [P.Printer]
                     -> IO ()
runVinapuPostgres host dbname user pwd sysId printers =  -- loadCase = 
    getConnection host dbname user pwd >>= \c ->
    LR.loadsAsMap c sysId >>= \loads ->
    NR.fetchNodesAsMap c sysId >>= \nodes ->
    ER.fetchElements c sysId nodes loads >>= \elx ->
    PR.fetchProject c sysId >>= \proj ->
    runVinapu elx (Map.elems nodes) printers (Just proj) >>
    close c >> 
    return ()

printLoad :: LR.LoadDTO -> IO ()
printLoad ld = putStrLn (show ld)

printLoadsForSystem :: String    -- ^ Database Host  
                       -> String -- ^ Database Name
                       -> String -- ^ Database User 
                       -> String -- ^ Database Password
                       -> Int    -- ^ System Id
                       -> IO ()
printLoadsForSystem host dbname user pwd sysId = 
    getConnection host dbname user pwd >>= \c ->
    LR.fetchLoads c sysId >>= \loads ->
    mapM_ printLoad loads >>
    close c >> 
    return ()


runVinapuXml :: X.Element 
                -> String  -- ^ Load Case
                -> [P.Printer]
                -> IO ()
runVinapuXml doc lc printers = do
    let loads = XL.createVinapuLoads doc
    let lcel = XE.loadCase doc lc
    let nodes = XN.createVinapuNodes lcel
    let elx = XE.createVinapuElements lcel nodes loads 
    runVinapu elx (Map.elems nodes) printers Nothing
    return ()
