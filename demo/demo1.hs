import qualified Vinapu.Nodes as N
import Vinapu.LoadSU (LoadSU(..))
import Vinapu.Loads (people,concreteSlab,DistLoad(..),LoadPair(..))
import qualified Vinapu.Elements as E
import qualified Vinapu.System as S
import Vinapu.Common (LimitState,ro2dec,partition)
import qualified Vinapu.Printers as P
import qualified Vinapu.Projects as PJ
import Vinapu.ElementResults (ElementResult(..),NodeResult(..),sumNode)
import qualified Vinapu.ElementResults as ER
import qualified Vinapu.Loads as L
import qualified Vinapu.LoadSU as LU

n1 = N.Node 1 (Just "A") 0 0 0 
n2 = N.Node 2 (Just "B") 3 0 0
n3 = N.Node 3 (Just "C") 5 0 0
n4 = N.Node 4 (Just "D") 10 0 0
n5 = N.Node 5 (Just "E") 15 0 0

nodes = [n1,n2,n3,n4]

conc = concreteSlab 200
lp1 = [people]  
lp2 = [conc,people,people]  

e1 = E.PlateElement 1 "A" n1 n2 lp2 0.5 7 
e2 = E.PlateElement 2 "B" n2 n4 lp1 0.5 2
e3 = E.PlateElement 3 "C" n4 n5 lp1 0.5 7
e4 = E.PlateElement 4 "D" n3 n5 lp1 0.5 7
e5 = E.PlateElement 5 "E" n1 n4 lp2 0.5 7

elx = [e1,e2,e3]
elx2 = [e2]

s1 = ER.sumNode elx2 n2
