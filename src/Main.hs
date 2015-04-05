{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}
-- #define RCS_DEMO

import GHC.Float (float2Double)
import System.Console.CmdLib -- (Attributes,Group,Help,ArgHelp,Default,RecordCommand)
import qualified Text.XML.Light as X 
import qualified Vinapu.System as S
import qualified Vinapu.Printers as P

data Main = Main { 
        h :: String,
        db :: String,
        u :: String,
        s :: Int,
        txt :: Bool,
        html :: Bool,
        o :: String
    }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
            h      %> [ Group "Database", Help "Database host", Default "xochitecatl" ] ,
            db     %> [ Group "Database", Help "Database name", Default "engineer" ] ,
            u      %> [ Group "Database", Help "Database user", Default "engineer" ] ,
            s      %> [ Group "System", Positional 0, Required True ] ,
            o      %> [ Group "File", Help "Output file name (if --txt or --html is set)", ArgHelp "FILENAME", Default "N/A" ] ,
            -- lc     %> [ Group "Load", Help "Load case", ArgHelp "LOADCASE", Default "default" ] ,
            txt    %> [ Group "File", Help "Output to text file compatible with pandoc" ] ,
            html   %> [ Group "File", Help "Output to html file compatible with pandoc" ] 
        ]

instance RecordCommand Main where
    mode_summary _ = "Vinapu Structural Load calculator"

main :: IO ()
main = getArgs >>= executeR Main {} >>= \opts -> 
        let printers | (html opts) == True = [P.StdoutPrinter,P.HtmlPrinter (o opts)]
                     | otherwise = [P.StdoutPrinter] in 
        S.runVinapuPostgres (h opts) (db opts) (u opts) (s opts) printers >>
        return ()

        
