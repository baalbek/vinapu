{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.StdoutPrinter where

import Vinapu.Printers (Printer,print) 
import Vinapu.ElementResults (ElementResult)

data StdoutPrinter = 
    StdoutPrinter {
        results :: [ElementResult]
    } deriving Show


instance Printer StdoutPrinter where
    print sdp =  
        putStrLn "Hi" >>
        return ()
