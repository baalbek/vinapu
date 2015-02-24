import qualified Vinapu.Nodes as N
import Vinapu.LoadSU ((<++>),LoadSU(..))
import Vinapu.Loads (people,concreteSlab,DistLoad(..),LoadPair(..))
import qualified Vinapu.Elements as E
import Vinapu.Common (LimitState,ro2dec,partition)

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

sumNode curElx n = let latn = E.unitLoadAtNode n in foldr (<++>) Nothing $ map latn curElx 

nodex = partition 2 1 nodes

[a,b] = head nodex

span1 = E.spans a b

spanned = filter span1 elx

load1a = sumNode spanned a
load1b = sumNode spanned b

