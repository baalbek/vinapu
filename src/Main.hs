{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}
-- #define RCS_DEMO

import System.Console.CmdLib -- (Attributes,Group,Help,ArgHelp,Default,RecordCommand)
import qualified Vinapu.System as S
import qualified Vinapu.Printers as P

data Main = Main { 
        h :: String,
        db :: String,
        u :: String,
        s :: Int,
        txt :: Bool,
        html :: Bool,
        o :: String,
        slo :: Bool
    }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
            h      %> [ Group "Database", Help "Database host", Default "xochitecatl2" ] ,
            db     %> [ Group "Database", Help "Database name", Default "engineer" ] ,
            u      %> [ Group "Database", Help "Database user", Default "engineer" ] ,
            s      %> [ Group "System", Positional 0, Required True ] ,
            o      %> [ Group "File", Help "Output file name (if --txt or --html is set)", ArgHelp "FILENAME", Default "N/A" ] ,
            -- lc     %> [ Group "Load", Help "Load case", ArgHelp "LOADCASE", Default "default" ] ,
            txt    %> [ Group "File", Help "Output to text file compatible with pandoc" ] ,
            html   %> [ Group "File", Help "Output to html file compatible with pandoc" ] ,
            slo    %> [ Group "System", Help "If set, print database loads for system s", Default False ] 
        ]

instance RecordCommand Main where
    mode_summary _ = "Vinapu Structural Load calculator"

main :: IO ()
main = getArgs >>= executeR Main {} >>= \opts ->
    let dbHost = (h opts)
        dbName = (db opts)
        dbUser = (u opts)
        sysId = (s opts) in
    if (slo opts) == True 
        then
            S.printLoadsForSystem dbHost dbName dbUser sysId  
        else 
            let printers | (html opts) == True = [P.StdoutPrinter,P.HtmlPrinter (o opts)]
                            | otherwise = [P.StdoutPrinter] in 
            S.runVinapuPostgres dbHost dbName dbUser sysId printers >>
            return ()

        
