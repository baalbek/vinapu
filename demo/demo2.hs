
import Vinapu.Loads (people,concreteSlab,ytong,DistLoad(..),LoadPair(..))
import qualified Vinapu.Nodes as N
import qualified Vinapu.Elements as E
import qualified Vinapu.System as S

n1 = N.Node "1" 0 0 
n2 = N.Node "2" 3 0 
n3 = N.Node "3" 5 0 
n4 = N.Node "4" 10 0 
n5 = N.Node "5" 15 0 

nodes = [n1,n2,n3,n4,n5]

conc = concreteSlab 200
snow = Snow 4.5 0.8
lp = LoadPair conc snow

e1 = E.PlateElement n1 n2 2.0 lp 0.5
e2 = E.PlateElement n2 n4 5.0 lp 0.5
e3 = E.PlateElement n4 n5 5.0 lp 0.5
e4 = E.PlateElement n3 n5 6.0 lp 0.5
e5 = E.PlateElement n1 n4 6.0 lp 0.5

elx = [e1,e2,e3,e4,e5]

--S.runVinapu elx nodes 

nod2 = [n1,n2] 
conc2 = concreteSlab 150
lp2 = LoadPair conc2 people
e6 = E.PlateElement n1 n2 6.0 lp2 0.5
elx2 = [e6]




