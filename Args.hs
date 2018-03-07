module Args where  

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe ( fromMaybe )
import Dimacs
import GraphData
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

data Flag
    = Input
    | Flow
    | Path
    deriving (Show, Eq)

options =
    [ Option ['i']  []   (NoArg Input)  "Print parsed input."
    , Option ['f']  []   (NoArg Flow)   "Print maximal flow of a path in graph."
    , Option ['v']  []   (NoArg Path)   "Print path with maximal flow."
    ]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv = 
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ford-fulkerson [OPTION...] file"

resolveOpts :: ([Flag], [String]) -> IO ()
resolveOpts (flags, other)
    | length other > 1      = print "Only one file argument is needed."
    | length other == 0     = print "Insert"
    | Input `elem` flags = parsedGraph >>= (\(InputData graph handle) -> printGraph graph >> hClose handle)
    | Path `elem` flags  = parsedGraph >>= (\(InputData graph handle) -> (printPath $ fst $ findMaxFlowPath graph) >> hClose handle)
    | Flow `elem` flags  = parsedGraph >>= (\(InputData graph handle) -> (print $ snd $ findMaxFlowPath graph) >> hClose handle)
        where
            parsedGraph = parseInput (head other)
{-- 
    data TInputData = InputData {
        graph :: TGraph,
        handle :: Handle
    }

    parseInput :: String -> IO TInputData

--}