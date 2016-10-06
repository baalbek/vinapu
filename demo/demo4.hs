{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))
import Control.Monad (mplus)
import Data.List (nub)

import Database.PostgreSQL.Simple (connectPostgreSQL,close)

import Vinapu.Common (partition)

import qualified Vinapu.Repos.NodeRepository as NR
import qualified Vinapu.Repos.ElementRepository as ER
import qualified Vinapu.Repos.LoadRepository as LR
import qualified Vinapu.Printers as P

{-
import qualified Vinapu.Nodes as N
import qualified Vinapu.Loads as L
import Vinapu.LoadSU (LoadSU(..),(<+>),(<++>))
import qualified Vinapu.Elements as E
import qualified Vinapu.System as S
import qualified Vinapu.Printers as P
import Vinapu.Common (LimitState,partition,getConnection)
-}

sysId = 2 

-- c = connectPostgreSQL "host='192.168.56.63' dbname='engineer2' user='engineer'"
c = connectPostgreSQL "host='172.17.0.01' dbname='engineer' user='engineer' password='ok'"

nx = c >>= \conn -> NR.fetchNodesAsMap conn sysId

nxel = nx >>= \nxx -> putStrLn (show (partition 2 1 (Map.elems nxx))) >> return ()

ex = c >>= \conn -> ER.fetchElementDTOs conn sysId

lx = c >>= \conn -> LR.loadsAsMap conn sysId

elx = c >>= \conn -> nx >>= \nxx -> lx >>= \lxx -> ER.fetchElements conn sysId nxx lxx

{-
wnx = c >>= \conn -> NR.fetchWNodesAsMap conn sysId

lx = c >>= \conn -> LR.loadsAsMap conn sysId

testme = lx >>= \x -> 
         let Just dtos = Map.lookup 25 x in
         putStrLn (show dtos)
         >> return ()

elx = c >>= \conn ->
      nx >>= \nm ->
      wnx >>= \wnm ->
      lx >>= \lm ->
      ER.fetchElements conn sysId nm wnm lm  

l1 = L.UniformDistLoad 76 L.DEAD_LOAD "Betongdekke" 2 3
l2 = L.UniformDistLoad 77 L.DEAD_LOAD "Tak" 3 4

loads = [l1,l2] 

runVinapu sysId = S.runVinapuPostgres host "engineer" "engineer" sysId [P.StdoutPrinter] 

prlo sysId = S.printLoadsForSystem host "engineer" "engineer" sysId 

n1 = N.Node 1 (Just "N1") 0 0 0 

n2 = N.Node 2 (Just "N2") 10 0 0 

snow = c >>= \conn -> LR.singleLoadsAsMap conn 1

comp = c >>= \conn -> LR.compositeLoadsAsMap conn 1

nx = c >>= \conn -> NR.fetchNodesAsMap conn 1

elx = c >>= \conn ->
      nx >>= \nm ->
      snow >>= \slm ->
      comp >>= \clm ->
      ER.fetchElements conn 1 nm slm clm  

runVinapu = S.runVinapuPostgres host "engineer" "engineer" 6 [P.StdoutPrinter] 

gn :: Int -> N.NodeMap -> N.Node
-}
