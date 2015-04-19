
module Vinapu.System where

import Text.Printf (printf)
import qualified Text.XML.Light as X 

import qualified Data.Map as Map

import Database.PostgreSQL.Simple (close)

import qualified Vinapu.Repos.ElementRepository as ER
import qualified Vinapu.Repos.LoadRepository as LR
import qualified Vinapu.Repos.NodeRepository as NR
import qualified Vinapu.Elements as E
import qualified Vinapu.Loads as L
import qualified Vinapu.Nodes as N
import qualified Vinapu.ElementResults as R
import qualified Vinapu.Printers as P
import Vinapu.Common (LimitState,partition,getConnection)

type NodeSpan = [N.Node]

collectSpan :: [E.Element] -> NodeSpan -> R.ElementResult
collectSpan elements nodeSpan = R.ElementResult nra nrb
    where [na,nb] = nodeSpan 
          span = E.spans na nb
          spanned = filter span elements
          nra = R.NodeResult na spanned
          nrb = R.NodeResult nb spanned

collectResults :: [E.Element] ->  [NodeSpan] -> [R.ElementResult]
collectResults elements nodeSpans = 
    let collectSpan' = collectSpan elements 
    in  map collectSpan' nodeSpans


runVinapu :: [E.Element]
             -> [N.Node]
             -> [P.Printer]
             -> IO ()
runVinapu elements nodes printers = 
    let nxp = partition 2 1 nodes 
        results = collectResults elements nxp in
    mapM_ (P.print results) printers >>
    return ()

runVinapuPostgres :: String    -- ^ Database Host  
                     -> String -- ^ Database Name
                     -> String -- ^ Database User 
                     -> Int    -- ^ System Id
                     -- -> Int    -- ^ Load Case
                     -> [P.Printer]
                     -> IO ()
runVinapuPostgres host dbname user sysId printers =  -- loadCase = 
    getConnection host dbname user >>= \c ->
    LR.loadsAsMap c sysId >>= \loads ->
    NR.fetchNodesAsMap c sysId >>= \nodes ->
    ER.fetchElements c sysId nodes loads >>= \elx ->
    runVinapu elx (Map.elems nodes) printers >>
    close c >> 
    return ()


{-
    getConnection host dbname user >>= \c ->
    LR.singleLoadsAsMap c sysId >>= \singLoads ->
    LR.compositeLoadsAsMap c sysId >>= \compLoads ->
    NR.fetchNodesAsMap c sysId >>= \nodes ->
    ER.fetchElements c sysId nodes singLoads compLoads >>= \elx ->
    runVinapu elx (Map.elems nodes) printers >>
    close c >> 
    return ()
-}
