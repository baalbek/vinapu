{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Vinapu.Projects where

data Project = 
    Project {
        projectName :: String
        ,locName :: String
        ,sysName :: String
    } deriving Show
