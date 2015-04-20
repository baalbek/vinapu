{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))
import Control.Monad (mplus)
import Data.List (nub)

import Database.PostgreSQL.Simple (connectPostgreSQL,close)

import qualified Vinapu.Repos.LoadRepository as LR
import qualified Vinapu.Repos.ElementRepository as ER
import qualified Vinapu.Repos.NodeRepository as NR
import qualified Vinapu.Nodes as N
import qualified Vinapu.Loads as L
import Vinapu.LoadSU (LoadSU(..),(<+>),(<++>))
import qualified Vinapu.Elements as E
import qualified Vinapu.System as S
import qualified Vinapu.Printers as P
import Vinapu.Common (LimitState,partition,getConnection)


c = connectPostgreSQL "host='xochitecatl2' dbname='engineer' user='engineer'"

nx = c >>= \conn -> NR.fetchNodesAsMap conn 12

lx = c >>= \conn -> LR.loadsAsMap conn 12

-- testMe = lm >>= (putStrLn . show) >> return ()

testme = lx >>= \x -> 
         let Just dtos = Map.lookup 25 x in
         putStrLn (show dtos)
         >> return ()

elx = c >>= \conn ->
      nx >>= \nm ->
      lx >>= \lm ->
      ER.fetchElements conn 12 nm lm  

l1 = L.UniformDistLoad 76 L.DEAD_LOAD "Betongdekke" 2 3
l2 = L.UniformDistLoad 77 L.DEAD_LOAD "Tak" 3 4

loads = [l1,l2] 

runVinapu sysId = S.runVinapuPostgres "xochitecatl2" "engineer" "engineer" sysId [P.StdoutPrinter] 

{-
snow = c >>= \conn -> LR.singleLoadsAsMap conn 1

comp = c >>= \conn -> LR.compositeLoadsAsMap conn 1

nx = c >>= \conn -> NR.fetchNodesAsMap conn 1

elx = c >>= \conn ->
      nx >>= \nm ->
      snow >>= \slm ->
      comp >>= \clm ->
      ER.fetchElements conn 1 nm slm clm  

runVinapu = S.runVinapuPostgres "xochitecatl2" "engineer" "engineer" 6 [P.StdoutPrinter] 

gn :: Int -> N.NodeMap -> N.Node
-}
