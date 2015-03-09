{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards  #-}
module Vinapu.Tables where

import Text.Printf (printf)
import qualified Vinapu.LoadSU as LU

data Row = 
    Row {
        col1 :: String,
        col2 :: String,
        col3 :: String
    } 
    | EmptyRow {
    }deriving Show

data Table = 
    Table {
        title :: String,
        rows :: [Row]
    } deriving Show

load2Row :: String -> Maybe LU.LoadSU -> Row
load2Row titl lsu = 
    case lsu of Nothing -> EmptyRow
                Just lsu' -> let sload = (LU.service lsu')
                                 uload = (LU.ultimate lsu') in
                    Row titl (printf "%.2f" sload) (printf ".%2f" uload)

row2Html :: Row -> String
row2Html Row { col1,col2,col3 } = printf "<tr><td>%s</td><td>%s</td><td>%s</td></tr>" col1 col2 col3 
row2Html (EmptyRow) = "<tr><td>-</td><td>-</td><td>-</td></tr>" 

table2Html :: Table -> [String]
table2Html Table { title,rows } = printf "<p>%s</p>" title : "<table><td>-</td><td>Bruksgrense</td><td>Bruddgrense</td>" : map row2Html rows ++ ["</table>"]
         
