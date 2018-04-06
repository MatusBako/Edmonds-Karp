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

data TArgData = ArgData {
    inputData :: TInputData,
    argFlags :: [Flag]
}

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


loadAndExecute :: ([Flag], [String]) -> IO ()
loadAndExecute (_, other)
    | length other > 1          = error "Only one file argument is needed."
loadAndExecute (flags, other)   = parsedGraph >>= (
        \(InputData graph handle) -> resolveOpts (ArgData (InputData graph handle) flags)
    )
    where
        parsedGraph = parseInput (if null other then "" else head other)


-- (ArgData (InputData graph handle) flags)

resolveOpts :: TArgData -> IO ()
--resolveOpts (ArgData (InputData graph handle) [])
resolveOpts (ArgData (InputData _ handle) [])   = hClose handle
resolveOpts (ArgData (InputData graph handle) flags)
    | (head flags) == Input = printGraph graph >> resolveOpts (ArgData (InputData graph handle) (tail flags))
    | (head flags) == Path  = (printPath $ fst $ findMaxFlowPath graph) >> resolveOpts (ArgData (InputData graph handle) (tail flags))
    | (head flags) == Flow  = (print $ snd $ findMaxFlowPath graph) >> resolveOpts (ArgData (InputData graph handle) (tail flags))
resolveOpts _   = error "Error while parsing arguments."
