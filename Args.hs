module Args where  

import Dimacs
import GraphData
import System.Console.GetOpt
import System.IO

data Flag
    = Input
    | Flow
    | Path
    deriving (Show, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['i']  []   (NoArg Input)  "Print parsed input."
    , Option ['f']  []   (NoArg Flow)   "Print maximal flow of a path in graph."
    , Option ['v']  []   (NoArg Path)   "Print path with maximal flow."
    ]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv = 
    case getOpt Permute options argv of
        (o,n,[]  ) -> return $ unifyOpts (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ford-fulkerson [OPTION...] file"

unifyOpts :: ([Flag], [String]) -> ([Flag], [String])
unifyOpts ([], other) = ([], other)
unifyOpts (flags, other) = (unify flags, other)

unify :: Eq a => [a] -> [a]
unify [] = []
unify (x:xs)
    | x `elem` xs   = unify xs
    | otherwise     = x:unify xs

resolveOpts :: ([Flag], [String]) -> IO ()
resolveOpts (_, other)
    | length other > 1      = error "Only one file argument is needed."
resolveOpts ([], other)     = parsedGraph >>= (\(InputData _ handle) -> hClose handle)
    where
        parsedGraph = parseInput (if null other then "" else head other)
resolveOpts (flags, other)
    | (head flags) == Input = parsedGraph >>= (\(InputData graph handle) -> printGraph graph >> hClose handle >> resolveOpts (tail flags, other))
    | (head flags) == Path  = parsedGraph >>= (\(InputData graph handle) -> (printPath $ fst $ findMaxFlowPath graph) >> hClose handle >> resolveOpts (tail flags, other))
    | (head flags) == Flow  = parsedGraph >>= (\(InputData graph handle) -> (print $ snd $ findMaxFlowPath graph) >> hClose handle >> resolveOpts (tail flags, other))
        where
            parsedGraph = parseInput (if null other then "" else head other)
resolveOpts (_, _)          = error "Error while parsing arguments."
