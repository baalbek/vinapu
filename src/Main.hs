{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}
-- #define RCS_DEMO

import qualified Text.XML.Light as X 
import Text.Printf (printf)

import System.Console.CmdArgs (cmdArgs,Data,Typeable,typ,def,groupname,(&=))
import qualified Vinapu.System as S
import qualified Vinapu.Printers as P

data CmdLine = 
    CmdLine {
        isxml :: Bool 
        ,ishtml :: Bool 
        ,htmlpath :: String
        ,htmlname :: String
        ,xml :: String
        ,host :: String
        ,dbname :: String
        ,user :: String
        ,password:: String
        ,system :: Int
        ,loadcase :: Int} deriving (Show, Data, Typeable)

cmdLine = CmdLine {
        isxml = False &= groupname "Input/output"
        --,host = "192.168.56.63" &= groupname "Database"
        ,host = "172.17.0.1" &= groupname "Database"
        ,dbname = "engineer" &= groupname "Database"
        ,user = "engineer" &= groupname "Database"
        ,password = "ok" &= groupname "Database"
        ,xml = "/home/rcs/opt/haskell/vinapu/demo/laster.xml" &= groupname "Input/output"
        ,htmlpath = "/home/rcs/opt/haskell/vinapu/demo" &= groupname "Input/output"
        ,htmlname = "laster.html" &= groupname "Input/output"
        ,system = 2 &= groupname "System"
        ,loadcase = 1 &= groupname "System"
        ,ishtml = False &= groupname "Input/output" }

getPrinters :: CmdLine -> IO [P.Printer]
getPrinters opts =
   case (ishtml opts) of  
        True -> let htmlresult = printf "%s/%s" (htmlpath opts) (htmlname opts) 
                    sysId = (system opts) in 
                    return [P.StdoutPrinter,P.HtmlPrinter htmlresult] 
        False -> return [P.StdoutPrinter]
    
        
runDbSystem :: CmdLine -> IO ()
runDbSystem opts = 
    let dbHost = (host opts)
        dbName = (dbname opts)
        dbUser = (user opts)
        dbPassword = (password opts)
        sysId = (system opts) in 
    getPrinters opts >>= \printers ->
        putStrLn (show opts) >>
        S.runVinapuPostgres dbHost dbName dbUser dbPassword sysId printers >>
        return ()

runXmlSystem :: CmdLine -> IO ()
runXmlSystem opts = 
    putStrLn ("\nXml file: " ++ (xml opts)) >>
    readFile (xml opts) >>= \s ->
        let lc = show (loadcase opts)
            printers = [P.StdoutPrinter] in
            putStrLn ("\nLoad case: " ++ lc ++ "\n") >> 
            case X.parseXMLDoc s of
                Nothing -> error "Failed to parse xml"
                Just doc -> S.runVinapuXml doc lc printers
            >> return ()
        

main :: IO ()
main = cmdArgs cmdLine >>= \opts -> 
    if (isxml opts) == False 
        then
            runDbSystem opts
        else
            runXmlSystem opts

{-        
    let dbHost = (host opts)
        dbName = (dbname opts)
        dbUser = (user opts)
        sysId = (system opts) in
    S.printLoadsForSystem dbHost dbName dbUser sysId >>
    return ()

    if (slo opts) == True 
        then
            S.printLoadsForSystem dbHost dbName dbUser sysId  
        else 
            let printers | (html opts) == True = [P.StdoutPrinter,P.HtmlPrinter (o opts)]
                            | otherwise = [P.StdoutPrinter] in 
            S.runVinapuPostgres dbHost dbName dbUser sysId printers >>
            return ()
-} 
        
