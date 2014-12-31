{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit (runTestTT)
import Test.HUnit.Base (Counts)

import TestLoads (testLoads)
import TestElements (testElements)

main :: IO Counts
main = do
    runTestTT testLoads
    runTestTT testElements
