import qualified Vinapu.Nodes as N
import Vinapu.LoadSU ((<+>),(<++>),LoadSU(..))
import Vinapu.Loads (people,concreteSlab,DistLoad(..),LoadPair(..))
import qualified Vinapu.Elements as E
import qualified Vinapu.System as S
import Vinapu.Common (LimitState,ro2dec,partition)
import qualified Vinapu.Printers as P
import Vinapu.ElementResults (ElementResult(..),NodeResult(..),sumNode)
import qualified Vinapu.ElementResults as ER
import qualified Vinapu.Loads as L
import qualified Vinapu.LoadSU as LU

n1 = N.Node 1 (Just "A") 0 0 0 
n2 = N.Node 2 (Just "B") 3 0 0
n3 = N.Node 3 (Just "C") 5 0 0
n4 = N.Node 4 (Just "D") 10 0 0
n5 = N.Node 5 (Just "E") 15 0 0

nodes = [n1,n2,n3,n4,n5]

conc = concreteSlab 200
lp = [conc,people]  

e1 = E.PlateElement 1 "A" n1 n2 lp 0.5 7 
e2 = E.PlateElement 2 "B" n2 n4 lp 0.5 7
e3 = E.PlateElement 3 "C" n4 n5 lp 0.5 7
e4 = E.PlateElement 4 "D" n3 n5 lp 0.5 7
e5 = E.PlateElement 5 "E" n1 n4 lp 0.5 7

-- elx = [e1,e2,e3,e4,e5]
--
elx = [e1,e2]

-- sumNode curElx n = let latn = E.unitLoadAtNode n in foldr (<++>) Nothing $ map latn curElx 

nodex = partition 2 1 nodes

{-
[a,b] = head nodex
span1 = E.spans a b
spannex = filter span1 elx
load1a = sumNode spannex a
load1b = sumNode spannex b
-}

elres = S.collectSpan elx (head nodex)

elres2 = S.collectResults elx nodex

nr1' = ER.nr1 (head elres2)

prn = P.print elres2 (P.HtmlPrinter "demo.html") 

r1 = P.htmlElementResult (head elres2)

Just lp1 = E.loadPairAtNode (head nodes) e1

lu1 = L.loadSU1 (L.liveLoad lp1)

lu1s = LU.service lu1 

lu1u = LU.ultimate lu1 

