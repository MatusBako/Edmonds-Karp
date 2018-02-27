import Dimacs

import GraphData
import Text.Read
import System.IO
import qualified Data.Map as Map

main = do
    --parseInput "in" >>= (\x -> (print $ graph x) >> (hClose $ handle x))
    let n = BfsNode 3 [Edge 1 3]
    let g = (Graph 5 8 1 5 [Edge 1 2, Edge 1 3, Edge 3 2, Edge 3 5, Edge 4 5, Edge 4 2, Edge 5 3, Edge 3 1] 
                (Map.fromList [(Edge 1 2,4), (Edge 1 3,5), (Edge 3 2,3), (Edge 3 5,6), (Edge 4 5,2), (Edge 5 2,4), 
            (Edge 5 3,6), (Edge 3 1,2)] ))

    let m = Map.empty

    print $ foldl (\m (Edge f s) -> Map.insert (Edge f s) f m) m (edges g)

    --print $ map ((\current neighbour -> BfsNode neighbour ((path current)++[neighbour])) (n)) $ getNodeNeighbourds (edges g) (node n)
    --print $ bfs g 5
