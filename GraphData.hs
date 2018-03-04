module GraphData where

import qualified Data.Map as Map
import Data.List as List
import Debug.Trace

------- DATA TYPES ---------

type Node =  Word
data TEdge = Edge Node Node deriving(Eq, Ord)
type Path = [TEdge]

instance Show TEdge where
  show (Edge a b ) = show a ++ "->" ++ show b

data TBfsNode = BfsNode {
    node :: Node,
    path :: Path
}

instance Show TBfsNode where
    show (BfsNode n p) = "BfsNode: " ++ show n ++ " " ++ show p ++ "\n"

createEdges :: [[Word]] -> [TEdge]
createEdges []          = []
createEdges (x:xs) 
    | length x == 2   = Edge (head x) (x!!1) : createEdges xs
    | otherwise         = error ("Prvok zoznamu " ++ show x ++ " nema dva prvky na vytvorenie hrany.")


data TGraph = Graph {
    nodeCount :: Word,
    edgeCount :: Word,
    source :: Word,
    target :: Word,
    edges :: [TEdge],
    capacity :: Map.Map TEdge Word
}   deriving (Show, Eq) 

data TResidualData = ResidualData {
    resEdges :: [TEdge],
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
getNodeNeighbourds edges node = 
    map (\(Edge x y) -> y) $ filter (\(Edge x y) -> x == node) edges


------- OPERATIONS ---------

invertEdge :: TEdge -> TEdge
invertEdge (Edge x y) = Edge y x

bfs ::  TGraph -> Node -> Path
bfs g n = findBfs g n [BfsNode (source g) []] []

findBfs :: TGraph -> Node -> [TBfsNode] -> [Node] -> Path
findBfs _ _ [] _ = []
findBfs (Graph nCnt  eCnt src tar edges cap) wanted (current:xs) explored 
    | node current `elem` explored  = findBfs (Graph nCnt  eCnt src tar edges cap) wanted xs explored
    | wanted == node current        = path current
    | otherwise                     = findBfs (Graph nCnt  eCnt src tar edges cap) wanted newQ newExplored
        where 
            newExplored = explored ++ [node current] 
            newQ = xs ++ 
                map (\x -> BfsNode x (path current ++[Edge (node current) x])) 
                (filter (`notElem` explored)
                $ getNodeNeighbourds edges (node current))

getPathCapacity :: TGraph -> Path -> Word
getPathCapacity (Graph nCnt  eCnt src tar edges capacities) path = minimum $ map 
    (\edge -> fromMaybe (error "Edge in path does not have capacity.")
        (Map.lookup edge capacities)
    ) path

getPathFlow :: Map.Map TEdge Word -> Path -> Word
getPathFlow flowMap path = minimum $ map 
    (\edge -> fromMaybe (error "Edge in path does not have capacity.")
        (Map.lookup edge flowMap)
    ) path

increaseFlow :: Map.Map TEdge Word -> Path -> Word -> Map.Map TEdge Word
increaseFlow flowMap path newFlow 
    | newFlow == 0 =    error "Increasing flow by 0!"
    | otherwise =       foldl (\flowMap edge -> case Map.lookup edge flowMap of 
                            Nothing -> error "Edge in flow map does not have capacity."
                            Just prevFlow -> Map.insert edge (prevFlow + newFlow) flowMap
                            ) flowMap path

createResidualGraph :: TGraph -> Word -> Path -> TGraph
createResidualGraph (Graph nCnt eCnt src tar edges graphCapacities) flow path = 
    Graph nCnt eCnt src tar (Map.keys $ capacities residualData) (Map.filter (>0) $capacities residualData)
    where residualData = foldl (\(ResidualData edges newCapacities) edge -> if not $ Map.member edge graphCapacities then error ("Edge capacity not found." ++ show edge) else
                case not $ Map.member (invertEdge edge) graphCapacities of
                    False -> ResidualData edges newCapacities
                        where 
                            newCapacities = Map.insert (invertEdge edge) flow -- increase opposite edge capacity
                                $ Map.adjust (subtract flow) edge newCapacities   -- decrease capacity

                    True -> ResidualData edges newCapacities
                        where
                            newCapacities = Map.adjust (subtract flow) edge newCapacities   -- decrease capacity
            ) (ResidualData edges graphCapacities) path

--F[u, v] = F[u, v] - m       //This is reducing the residual capacity of the augmenting path
--F[v, u] = F[v, u] + m

{--
foldl (-) 5 [1,2,3]
5 - 1 - 2 - 3 = -1

foldr (-) 5 [1,2,3]
1 - (2 - (3 - 5)) = 1 - (2 - (-2)) = 1 - 4 = -3
--}
