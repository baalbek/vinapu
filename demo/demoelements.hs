
import Vinapu.Loads (people,concreteSlab,ytong,DistLoad(..),LoadPair(..))
import qualified Vinapu.Nodes as N
import qualified Vinapu.Elements as E
import qualified Vinapu.System as S
import qualified Vinapu.Loads as L
import qualified Vinapu.LoadSU as LU
import Vinapu.Common (radians)
import qualified Vinapu.Repos.ElementRepository as ER

n1 = N.Node 1 Nothing 0 0 0
n2 = N.Node 2 Nothing 1 0 0
n3 = N.Node 3 Nothing 2 0 0
n4 = N.Node 4 Nothing 3 0 0
n5 = N.Node 5 Nothing 4 0 0

conc = concreteSlab 200
loads = [conc,people]

t1 = E.TrapezoidPlateElement 1 "Trapez 1" n1 n5 loads 0.5 0 10
