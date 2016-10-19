{-# LANGUAGE NamedFieldPuns #-}

import Text.Printf (printf)
import Control.Monad (liftM)
import Control.Monad.Reader (Reader,runReader,ask)
import Database.PostgreSQL.Simple (Connection,close)
import Vinapu.Common (getConnection)
import qualified Vinapu.ElementResults as R
import qualified Vinapu.System as S
import qualified Vinapu.Projects as PJ
import qualified Vinapu.Printers as P
import qualified Vinapu.Repos.ProjectRepository as PR
import qualified Vinapu.Repos.LoadRepository as LR
import qualified Vinapu.Repos.NodeRepository as NR
import qualified Vinapu.Repos.ElementRepository as ER

html :: Int -> IO String 
html v = putStrLn "Hi!" >> putStrLn (show v) >> readFile "/home/rcs/opt/haskell/vinapu/resources/tpl.html" 

yax :: Reader Int (IO String)
yax = ask >>= \x -> return (html x)
-- yax  = ask >>= \x -> return (concat ((show x) : ["8"])) :: Reader Int String 


-- c = connectPostgreSQL "host='172.17.0.01' dbname='engineer' user='engineer' password='ok'"

host = "172.17.0.01"
dbname = "engineer"
user = "engineer"
pwd = "ok"

erx = S.elementResultsSysId host dbname user pwd 2 

geoSys = getConnection host dbname user pwd >>= \c -> PR.fetchGeoSystems c 1 
lx sysId = getConnection host dbname user pwd >>= \c -> LR.fetchLoads c sysId
lxm sysId = getConnection host dbname user pwd >>= \c -> LR.loadsAsMap c sysId
nx sysId = getConnection host dbname user pwd >>= \c -> NR.fetchNodes c sysId
nxm sysId = getConnection host dbname user pwd >>= \c -> NR.fetchNodesAsMap c sysId
ex sysId = getConnection host dbname user pwd >>= \c -> 
                                    lxm sysId >>= \lxm' ->
                                    nxm sysId >>= \nxm' ->
                ER.fetchElements c sysId nxm' lxm'

yux projId = 
    let conn = getConnection host dbname user pwd in 
        conn                        >>= \c      -> 
        PR.fetchGeoSystems c projId >>= \geoSys ->
            let elres = S.elementResultsSysId' conn
                result = mapM (elres . PJ.oid) geoSys in
            (putStrLn . show) geoSys >> (close c) >> result >>= return . concat

sys = S.elementResultsSysId host dbname user pwd
pid = S.elementResultsProjId host dbname user pwd

printMe = pid 1 >>= \x -> S.printElementResults x [P.StdoutPrinter]

inspect' :: R.ElementResult -> IO () 
inspect' (R.ElementResult nr1 nr2 p) =  
    let prn = putStrLn . show 
        Just p' = p in 
    putStrLn (printf "------------------ %s ---------------" (PJ.sysName p')) >>
    prn nr1 >>  
    prn nr2 >>  
    return ()

inspect :: [R.ElementResult] -> IO ()
inspect e =
    mapM inspect' e >> return ()


