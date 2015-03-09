{-# LANGUAGE NamedFieldPuns,RecordWildCards #-}
module Vinapu.XML.Common where

import Data.Maybe (fromJust)
import qualified Text.XML.Light as X 
import qualified Data.List as LU

toQName :: String -> X.QName
toQName s = X.QName s Nothing Nothing

createAttr :: String -> String -> X.Attr
createAttr qname value = X.Attr (toQName qname) value

xmlAttr :: String -> X.Element -> Maybe String 
xmlAttr s el = X.findAttr (toQName s) el

xmlElement :: String -> X.Element -> Maybe X.Element
xmlElement s doc = X.findElement (X.unqual s) doc

xmlElements :: String -> X.Element -> [X.Element]
xmlElements s doc = X.findElements (X.unqual s) doc

findElementForAttr :: [X.Element] 
                      -> String    -- ^ QName
                      -> String    -- ^ Attribute value 
                      -> Maybe X.Element
findElementForAttr elx attName attVal = LU.find lookFn elx
    where lookFn :: X.Element -> Bool
          lookFn x = let a = xmlAttr attName x in 
            case a of Nothing -> False
                      Just a' -> a' == attVal

a2d :: X.Element -> String -> Double
a2d el attrName = result 
    where Just result = xmlAttr attrName el >>= (\s -> Just (read s)) :: Maybe Double
