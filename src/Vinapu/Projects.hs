{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Vinapu.Projects where

data Project = 
    Project {
        projectId :: Int
        ,projectName :: String
        ,locId :: Int 
        ,locName :: String
        ,sysId :: Int
        ,sysName :: String
    } deriving Show

newtype GeoSystem = 
    GeoSystem {
        oid :: Int
    } deriving Show
