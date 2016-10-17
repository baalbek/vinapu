
import Control.Monad (liftM)

import Text.StringTemplate

import qualified Vinapu.Nodes as N
import Vinapu.Loads (people,concreteSlab)
import qualified Vinapu.Elements as E
import qualified Vinapu.System as S
import qualified Vinapu.Printers as P

import Vinapu.Common (partition)

t = newSTMP "Hello $name$" :: StringTemplate String

t1 = render $ setAttribute "name" "Whatever sfdsf re lllJoe\n\nsfsder" t

tx = renderf (newSTMP "hello $names;separator='; '$" :: StringTemplate String) ("names","joe") ("names", "jeff") ("names","mort"):: String 

html :: IO String 
html = readFile "/home/rcs/opt/haskell/vinapu/resources/tpl.html" 

doHtml :: [String] -> IO String
doHtml eres = html >>= \hx ->
    return (renderf (newSTMP hx :: StringTemplate String)  ("projectName", "HWWWWERWES!") ("elementResults", eres) :: String)

n1 = N.Node 1 (Just "A") 0 0 0 
n2 = N.Node 2 (Just "B") 3 0 0
n3 = N.Node 3 (Just "C") 5 0 0
n4 = N.Node 4 (Just "D") 10 0 0
n5 = N.Node 5 (Just "E") 15 0 0

lp1 = [people]  

nodes = [n1,n2,n3]

e1 = E.PlateElement 1 "A" n1 n2 lp1 0.5 7 
e2 = E.PlateElement 2 "B" n2 n3 lp1 0.5 2

elx = [e1,e2]

ns = partition 2 1 nodes

ex = S.collectResults elx ns Nothing

erx = map (unlines . P.htmlElementResult) ex

t3 = doHtml erx        -- render $ setAttribute "elementResults"  erx
