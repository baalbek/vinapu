{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}
-- #define RCS_DEMO

import GHC.Float (float2Double)
import System.Console.CmdLib -- (Attributes,Group,Help,ArgHelp,Default,RecordCommand)
import qualified Text.XML.Light as X 
import qualified Vinapu.System as S

data Main = Main { 
        f :: String,
        lc :: String,
        txt :: Bool,
        html :: Bool,
        o :: String
        -- d :: Float
    }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
            f      %> [ Group "File", Help "Input XML file", ArgHelp "FILENAME", Default "/home/rcs/opt/haskell/vinapu/demo/demo.xml" ] ,
            o      %> [ Group "File", Help "Output file name (if --txt or --html is set)", ArgHelp "FILENAME", Default "N/A" ] ,
            lc     %> [ Group "Load", Help "Load case", ArgHelp "LOADCASE", Default "default-case" ] ,
            txt    %> [ Group "File", Help "Output to text file compatible with pandoc" ] ,
            html   %> [ Group "File", Help "Output to html file compatible with pandoc" ] 
            -- d      %> [ Group "File", Help "Float", ArgHelp "VAL", Default (23 :: Float) ] 
        ]

instance RecordCommand Main where
    mode_summary _ = "Vinapu Structural Load calculator"

main :: IO ()
main = getArgs >>= executeR Main {} >>= \opts -> do
    s <- readFile (f opts)
    case X.parseXMLDoc s of
        Nothing -> error "Failed to parse xml"
        Just doc -> S.runVinapuXml doc (lc opts)
    return ()

