{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))
import Control.Monad (mplus)
import Data.List (nub)

import Database.PostgreSQL.Simple (connectPostgreSQL,close)

import Vinapu.Common (partition)

import qualified Vinapu.Repos.ProjectRepository as PR
import qualified Vinapu.Projects as P

sysId = 2 

-- c = connectPostgreSQL "host='192.168.56.63' dbname='engineer2' user='engineer'"
c = connectPostgreSQL "host='172.17.0.01' dbname='engineer' user='engineer' password='ok'"

proj = c >>= \conn -> PR.fetchProject conn 5
