{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module Vinapu.Printers where

class Printer a where
    print :: a -> IO ()

