{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}
-- #define RCS_DEMO

import System.Console.CmdArgs (cmdArgs,Data,Typeable,typ,def,groupname,(&=))

import qualified Text.XML.Light as X 
import qualified Vinapu.System as S
import qualified Vinapu.Printers as P

data CmdLine = 
    CmdLine {
        xml :: String,
        loadcase :: String } deriving (Show, Data, Typeable)
 
cmdLine = CmdLine {
        xml = "/home/rcs/opt/haskell/vinapu/demo/laster.xml" &= groupname "System",
        loadcase = "default" &= groupname "System"}

main = cmdArgs cmdLine >>= \opts -> 
    putStrLn ("\nXml file: " ++ (xml opts)) >>
    readFile (xml opts) >>= \s ->
        let lc = loadcase opts 
            printers = [P.StdoutPrinter] in
            putStrLn ("\nLoad case: " ++ lc ++ "\n") >> 
            case X.parseXMLDoc s of
                Nothing -> error "Failed to parse xml"
                Just doc -> S.runVinapuXml doc lc printers
            >> return ()
        
{-
data Main = Main { 
        f :: String,
        lc :: String,
        txt :: Bool,
        html :: Bool,
        o :: String
    }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
            f      %> [ Group "File", Positional 0, Default "/home/rcs/opt/haskell/vinapu/demo/demo.xml" ] ,
            o      %> [ Group "File", Help "Output file name (if --txt or --html is set)", ArgHelp "FILENAME", Default "N/A" ] ,
            lc     %> [ Group "Load", Help "Load case", ArgHelp "LOADCASE", Default "default" ] ,
            txt    %> [ Group "File", Help "Output to text file compatible with pandoc" ] ,
            html   %> [ Group "File", Help "Output to html file compatible with pandoc" ] 
            -- d      %> [ Group "File", Help "Float", ArgHelp "VAL", Default (23 :: Float) ] 
        ]

instance RecordCommand Main where
    mode_summary _ = "Vinapu Structural Load calculator"

main :: IO ()
main = getArgs >>= executeR Main {} >>= \opts -> 
    putStrLn (f opts) >> return ()
    readFile (f opts) >>= \s ->
        let loadCase = lc opts 
            printers | (html opts) == True = [P.StdoutPrinter,P.HtmlPrinter (o opts)]
                        | otherwise = [P.StdoutPrinter] in 
            case X.parseXMLDoc s of
                Nothing -> error "Failed to parse xml"
                Just doc -> S.runVinapuXml doc (lc opts) printers
            >> return ()
-}



        
