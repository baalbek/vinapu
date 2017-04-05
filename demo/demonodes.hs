
import qualified Vinapu.Nodes as N
import qualified Vinapu.Elements as E
import Vinapu.Loads (people,concreteSlab,ytong,DistLoad(..),LoadPair(..))

n1 = N.Node 1 (Just "A1") 0 0 0
n2 = N.Node 2 (Just "A2") 0 10 0
n3 = N.Node 3 (Just "A3") 0 15 0
n4 = N.Node 4 (Just "B1") 5 0 0
n5 = N.Node 5 (Just "B2") 5 10 0
n6 = N.Node 6 (Just "B3") 5 15 0
n7 = N.Node 7 (Just "B4") 5 20 0
n8 = N.Node 8 (Just "B5") 5 20 0

conc = concreteSlab 200
loads = [conc,people]

e1 = E.PlateElement 1 "Trapez 1" n6 n4 loads 0.5 10
