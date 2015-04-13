{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mplus)

import Database.PostgreSQL.Simple (connectPostgreSQL,close)

import qualified Vinapu.Repos.LoadRepository as LR
import qualified Vinapu.Repos.ElementRepository as ER
import qualified Vinapu.Repos.NodeRepository as NR
import qualified Vinapu.Nodes as N
import qualified Vinapu.Loads as L
import qualified Vinapu.Elements as E
import qualified Vinapu.System as S
import qualified Vinapu.Printers as P
import Vinapu.Common (LimitState,partition,getConnection)


c = connectPostgreSQL "host='xochitecatl2' dbname='engineer' user='engineer'"

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
gn key nm = let Just result = Map.lookup key nm in result
