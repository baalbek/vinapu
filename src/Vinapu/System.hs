
module Vinapu.System where

import qualified Text.XML.Light as X 
import qualified Data.Map as Map

import Database.PostgreSQL.Simple (Connection,close)

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

elementResults ::   [E.Element]
                    -> [N.Node]
                    -> Maybe PJ.Project
                    -> IO [R.ElementResult]  
elementResults elements nodes proj = 
    let nxp = partition 2 1 nodes 
        results = collectResults elements nxp proj in
    -- mapM_ (P.print results) printers >>
    return results

elementResultsSysId' :: IO Connection
                        -> Int    -- ^ System Id
                        -> IO [R.ElementResult]  
elementResultsSysId' conn sysId = 
    conn                                    >>= \c      ->
    LR.loadsAsMap c sysId                   >>= \loads  ->
    NR.fetchNodesAsMap c sysId              >>= \nodes  ->
    ER.fetchElements c sysId nodes loads    >>= \elx    ->
    PR.fetchProject c sysId                 >>= \proj   ->
    elementResults elx (Map.elems nodes) (Just proj) 

elementResultsSysId :: String   -- ^ Database Host  
                     -> String  -- ^ Database Name
                     -> String  -- ^ Database User 
                     -> String  -- ^ Database password 
                     -> Int     -- ^ System Id
                     -> IO [R.ElementResult]  
elementResultsSysId host dbname user pwd sysId =  
    let c = getConnection host dbname user pwd   
        result = elementResultsSysId' c sysId in 
            c >>= close >> result

elementResultsProjId :: String   -- ^ Database Host  
                     -> String  -- ^ Database Name
                     -> String  -- ^ Database User 
                     -> String  -- ^ Database password 
                     -> Int     -- ^ Project Id
                     -> IO [R.ElementResult]  
elementResultsProjId host dbname user pwd projId =  
    let conn = getConnection host dbname user pwd in 
        conn                        >>= \c      -> 
        PR.fetchGeoSystems c projId >>= \geoSys ->
            let elres = elementResultsSysId' conn
                result = mapM (elres . PJ.oid) geoSys in
            (close c) >> result >>= return . concat

printElementResults ::  [R.ElementResult]
                        -> [P.Printer]
                        -> IO ()
printElementResults elres printers = 
    putStrLn "Printing..." >> (putStrLn . show .length) elres >>
    mapM (P.print elres) printers >> return ()





