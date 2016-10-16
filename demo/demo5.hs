{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))
import Control.Monad (mplus)
import Data.List (nub)

import Database.PostgreSQL.Simple (connectPostgreSQL,close)

import Vinapu.Common (partition)

import qualified Vinapu.Repos.ElementRepository as ER
import qualified Vinapu.Repos.LoadRepository as LR
import qualified Vinapu.Repos.NodeRepository as NR
import qualified Vinapu.Repos.ProjectRepository as PR
import qualified Vinapu.Printers as P
import qualified Vinapu.Projects as PJ
import qualified Vinapu.System as S

sysId = 2

-- c = connectPostgreSQL "host='192.168.56.63' dbname='engineer2' user='engineer'"
c = connectPostgreSQL "host='172.17.0.01' dbname='engineer' user='engineer' password='ok'"

sx  = c >>= \conn -> PR.fetchGeoSystems conn 1

ioproj = c >>= \conn -> PR.fetchProject conn sysId

ioloads = c >>= \conn -> LR.loadsAsMap conn sysId 

ionodes = c >>= \conn -> NR.fetchNodesAsMap conn sysId

ionspan = (partition 2 1) <$> Map.elems <$> ionodes

ioelx = c >>= \conn -> ioloads >>= \loads -> ionodes >>= \nodes -> ER.fetchElements conn sysId nodes loads 

printers = [P.HtmlPrinter "demo.html"]
printers2 = [P.StdoutPrinter,P.HtmlPrinter "demo.html"]

rx = ioloads >>= \loads -> ionodes >>= \nodes -> ioelx >>= \elements -> ioproj >>= \proj -> S.runVinapu elements (Map.elems nodes) printers2 (Just proj)

            
rx2 = ioelx >>= \elx -> ionspan >>= \nspan -> ioproj >>= \proj -> return (S.collectResults elx nspan (Just proj))


