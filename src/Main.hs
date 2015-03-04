{-# LANGUAGE NamedFieldPuns,RecordWildCards,CPP #-}

-- #define RCS_DEMO

import System.Environment (getArgs)
import qualified Text.XML.Light as X 
import qualified Vinapu.System as S

main :: IO ()
main = do
#ifdef RCS_DEMO
    s <- readFile "/home/rcs/opt/haskell/vinapu/demo/demo.xml"
#else
    [fileName] <- getArgs
    s <- readFile fileName 
#endif
    case X.parseXMLDoc s of
        Nothing -> error "Failed to parse xml"
        Just doc -> S.runVinapuXml doc
