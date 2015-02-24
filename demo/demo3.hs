
import Vinapu.Loads (people,concreteSlab,ytong,DistLoad(..),LoadPair(..))
import qualified Vinapu.Nodes as N
import qualified Vinapu.Elements as E
import qualified Vinapu.System as S

n1 = N.Node "1" 0 0 
n2 = N.Node "2" 3 0 
nx1 = [n1,n2]

n3 = N.Node "3" 0 0 
n4 = N.Node "4" 5 0
nx2 = [n3,n4]

n5 = N.Node "5" 0 0 
n6 = N.Node "6" 3 0
nx3 = [n5,n6]

yt = ytong 300
snow = Snow 4.5 0.8
lp = LoadPair yt snow

e1 = E.PlateElement n1 n2 (3.9+1.5) lp 0.5
e2 = E.PlateElement n3 n4 (3.6+3.6) lp 0.5
e3 = E.PlateElement n3 n4 (3.6+2.1) lp 0.5
e4 = E.PlateElement n5 n6 3 lp 0.5


elx1 = [e1]
elx2 = [e2]
elx3 = [e3]
elx4 = [e4]

r1 = S.runVinapu elx1 nx1
r2 = S.runVinapu elx2 nx2
r3 = S.runVinapu elx3 nx2
r4 = S.runVinapu elx4 nx3



