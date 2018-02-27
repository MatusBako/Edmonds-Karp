module GraphData where

import qualified Data.Map as Map

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
    | (length x) == 2   = [Edge (x!!0) (x!!1)] ++ createEdges xs
    | otherwise         = error ("Prvok zoznamu " ++ show x ++ " nema dva prvky na vytvorenie hrany.")


data TGraph = Graph {
    nodeCount :: Word,
    edgeCount :: Word,
    source :: Word,
    target :: Word,
    edges :: [TEdge],
    capacity :: Map.Map TEdge Word
}   deriving (Show) 

setGraphCounts :: TGraph -> Word -> Word -> TGraph
setGraphCounts g n e = g { nodeCount = n, edgeCount = e}

setGraphSource :: TGraph -> Word -> TGraph
setGraphSource g s = g { source = s}

setGraphDrain :: TGraph -> Word -> TGraph
setGraphDrain g t = g { target = t}

getNodeNeighbourds :: [TEdge] -> Node -> [Node]
getNodeNeighbourds edges node = 
    map (\(Edge x y) -> y) $ filter (\(Edge x y) -> x == (node)) (edges) 


--addEdge :: Graph -> Word -> Word -> Graph
-- addEdge g s t = g {edges = edges ++ Edge (s!!1) (t!!2)}

invertEdge :: TEdge -> TEdge
invertEdge (Edge x y) = Edge y x

bfs ::  TGraph -> Node -> Path
bfs g n = findBfs g n [BfsNode (source g) []] []

findBfs :: TGraph -> Node -> [TBfsNode] -> [Node] -> Path
findBfs _ _ [] _ = []
findBfs (Graph nCnt  eCnt src tar edges cap) wanted (current:xs) explored 
    | elem (node current) explored  = findBfs (Graph nCnt  eCnt src tar edges cap) wanted xs explored
    | (wanted == node current)      = (path current)
    | otherwise                     = findBfs (Graph nCnt  eCnt src tar edges cap) wanted newQ newExplored
        where 
            newExplored = explored ++ [node current] 
            newQ = xs ++ (map (\x -> BfsNode x ((path current)++[Edge (node current) x])) $ filter (\x -> not $ elem x explored) $ getNodeNeighbourds edges (node current))

getPathFlow :: TGraph -> Path -> Word
getPathFlow (Graph nCnt  eCnt src tar edges capacity) path = foldl1 min $ map 
                                                (\edge -> case (Map.lookup edge capacity) of 
                                                    Nothing -> error "Edge in path does not have capacity."
                                                    Just cap -> cap
                                                ) path

increaseFlow :: Map.Map TEdge Word -> Path -> Word -> Map.Map TEdge Word
increaseFlow flowMap path newFlow 
    | newFlow == 0 =    error "Increasing flow by 0!"
    | otherwise =       foldl (\fMap edge -> case (Map.lookup edge fMap) of 
                            Nothing -> error "Edge in flow map does not have capacity."
                            Just prevFlow -> Map.insert edge (prevFlow + newFlow) flowMap
                            ) flowMap path

createResidualGraph :: TGraph -> Map.Map TEdge Word -> TGraph
createResidualGraph (Graph nCnt eCnt src tar edges graphCapacity
    ) flowMap = (Graph nCnt  eCnt src tar (Map.keys newCapacities) newCapacities)
    where
        newCapacities = foldl (\cap edge -> case (Map.lookup edge graphCapacity) of 
                Nothing -> error ("Edge capacity not found." ++ show(edge))
                Just edgeCapacity -> case (Map.lookup edge flowMap) of 
                    Nothing -> error ("Edge flow not found." ++ show(edge))
                    Just edgeFlow -> Map.insert edge (newCapacity) cap
                        where newCapacity = edgeCapacity - edgeFlow + (Map.findWithDefault 0 (invertEdge edge) flowMap)       
            ) graphCapacity edges

{--
foldl (-) 5 [1,2,3]
5 - 1 - 2 - 3 = -1

foldr (-) 5 [1,2,3]
1 - (2 - (3 - 5)) = 1 - (2 - (-2)) = 1 - 4 = -3
--}
