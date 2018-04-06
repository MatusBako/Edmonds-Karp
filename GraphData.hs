module GraphData where

import Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord

------- DATA TYPES ---------

type Node =  Word
data TEdge = Edge Node Node deriving(Eq, Ord)
type Path = [TEdge]

instance Show TEdge where
  show (Edge a b ) = show a ++ "->" ++ show b

-- Record used for finding path to node using BFS.
data TBfsNode = BfsNode {
    bfsNode :: Node,
    bfsPath :: Path
}

instance Show TBfsNode where
    show (BfsNode n p) = "BfsNode: " ++ show n ++ " " ++ show p ++ "\n"

data TGraph = Graph {
    nodeCount :: Word,
    edgeCount :: Word,
    source :: Word,
    target :: Word,
    edges :: [TEdge],
    capacity :: Map.Map TEdge Word
}   deriving (Show, Eq) 

-- Record holding residual graph.
-- residualEdges     - edges of residual graph
-- capacities   - capacities of residual grapf
data TResidualData = ResidualData {
    residualEdges :: [TEdge],
    capacities :: Map.Map TEdge Word
}

------- SETTERS ---------

setGraphCounts :: TGraph -> Word -> Word -> TGraph
setGraphCounts g n e = g { nodeCount = n, edgeCount = e}

setGraphSource :: TGraph -> Word -> TGraph
setGraphSource g s = g { source = s}

setGraphDrain :: TGraph -> Word -> TGraph
setGraphDrain g t = g { target = t}

getNodeNeighbourds :: [TEdge] -> Node -> [Node]
getNodeNeighbourds graphEdges node = 
    map (\(Edge _ y) -> y) $ filter (\(Edge x _) -> x == node) graphEdges

------- OPERATIONS ---------

invertEdge :: TEdge -> TEdge
invertEdge (Edge x y) = Edge y x

initFlowMap :: [TEdge] -> Map.Map TEdge Word
initFlowMap mapEdges = Map.fromList $ map (\e -> (e, 0::Word)) mapEdges

printGraph :: TGraph -> IO ()
printGraph (Graph nCnt  eCnt src tar _ cap) =
    putStrLn ("p max " ++ show nCnt ++ " " ++ show eCnt ++ "\n"
        ++ "n " ++ show src ++ " s\n"
        ++ "n " ++ show tar ++ " t\n"
        ++ (init $ foldl1 (++) (
            map (\(Edge n1 n2, c)->
                "a " ++ show n1 ++ " " ++ show n2 ++ " " ++ show c ++"\n"
            ) $ Map.toList cap))
        )

printPath :: Path -> IO ()
printPath []    = putStrLn ""
printPath path  = putStrLn $ (\(Edge n1 _) -> show n1 ++ ",") (head path)
    ++ (\x -> take  (length x - 1 ) x ) (Prelude.foldl1 (++) $ Prelude.map (\(Edge _ n2) -> show n2 ++ ",") path)

-- Find path with maximal flow
findMaxFlowPath :: TGraph -> (Path, Word)
findMaxFlowPath graph = if null $ snd maxFlowStruct 
    then ([], 0) 
    else (maxPath, maxFlow)
        where
            maxFlowStruct = findMaxFlow graph [] (initFlowMap $ edges graph)
            flowMap = fst maxFlowStruct
            paths   = snd maxFlowStruct
            maxPath = getMaxPath paths flowMap
            maxFlow = getMaxFlow graph flowMap

-- Get path with maximal flow out of given paths
getMaxPath :: [Path] -> Map.Map TEdge Word -> Path
getMaxPath [] _ = error "No path to target found."
getMaxPath paths flowMap = maximumBy (comparing (getPathFlow flowMap)) paths

-- Get flow in given path
getMaxFlow :: TGraph -> Map.Map TEdge Word -> Word
getMaxFlow graph flowMap = sum $ Map.elems $ Map.filterWithKey (\(Edge _ e2) _ -> e2 == target graph) flowMap 

-- Graph -> Queue -> FlowMap -> (FlowMap, Paths)
findMaxFlow :: TGraph -> [Path] -> Map.Map TEdge Word -> (Map.Map TEdge Word, [Path])
findMaxFlow graph paths flowMap 
    | not $ null path   = findMaxFlow newGraph (path:paths) newFlowMap
    | null paths        = (Map.empty, [])
    | otherwise         = (Map.filter (>0) flowMap, paths)
        where
            path = bfs graph
            newFlowMap = increaseFlow flowMap path flowIncrease
            flowIncrease = getPathCapacity graph path
            newGraph = createResidualGraph graph (getPathCapacity graph path) path

-- Find path from source to target in graph using BFS.
bfs ::  TGraph -> Path
bfs g = findBfs g (target g) [BfsNode (source g) []] []

-- Recursive function computing BFS in graph.
-- Graph -> Target -> Queue -> Explored -> Path
findBfs :: TGraph -> Node -> [TBfsNode] -> [Node] -> Path
findBfs _ _ [] _ = []
findBfs (Graph nCnt  eCnt src tar edgs cap) wanted (current:xs) explored 
    | bfsNode current `elem` explored   = findBfs (Graph nCnt  eCnt src tar edgs cap) wanted xs explored
    | wanted == bfsNode current         = bfsPath current
    | otherwise                         = findBfs (Graph nCnt  eCnt src tar edgs cap) wanted newQ newExplored
        where 
            newExplored = bfsNode current:explored 
            newQ = xs ++ 
                map (\x -> BfsNode x (bfsPath current ++[Edge (bfsNode current) x])) 
                (filter (`notElem` explored)
                $ getNodeNeighbourds edgs (bfsNode current))

getPathCapacity :: TGraph -> Path -> Word
getPathCapacity (Graph _ _ _ _ _ edgeCapacities) path = minimum $ map 
    (\edge -> fromMaybe (error "Edge in path does not have capacity.")
        (Map.lookup edge edgeCapacities)
    ) path

getPathFlow :: Map.Map TEdge Word -> Path -> Word
getPathFlow flowMap path = minimum $ map 
    (\edge -> fromMaybe (error "Edge in path does not have capacity.")
        (Map.lookup edge flowMap)
    ) path

increaseFlow :: Map.Map TEdge Word -> Path -> Word -> Map.Map TEdge Word
increaseFlow flowMap path newFlow 
    | newFlow == 0 =    error "Increasing flow by 0!"
    | otherwise =       foldl (\fMap edge -> case Map.lookup edge fMap of 
                            Nothing -> error "Edge in flow map does not have capacity."
                            Just prevFlow -> Map.insert edge (newFlow + prevFlow) fMap
                            ) flowMap path

createResidualGraph :: TGraph -> Word -> Path -> TGraph
createResidualGraph (Graph nCnt _ src tar edgs graphCapacities) flow resPath = 
    Graph nCnt newEdgeCount src tar newEdges newCapacities
    where 
        newEdgeCount = fromIntegral $ length newEdges::Word
        newEdges = Map.keys newCapacities
        newCapacities = Map.filter (>0) (capacities residualData)
        residualData = foldl (\(ResidualData resEdges resCapacities) edge -> 
                if Map.notMember edge graphCapacities 
                then error ("Edge capacity not found." ++ show edge) 
                else ResidualData resEdges (Map.insertWith (+) (invertEdge edge) flow 
                    $ Map.adjust (subtract flow) edge resCapacities)
            ) (ResidualData edgs graphCapacities) resPath
