module GraphTests where

import Test.HUnit
import Data.Map as Map
import GraphData

edges1 = [Edge 1 2, Edge 1 3, Edge 3 2, Edge 3 5, Edge 4 5, Edge 4 2, Edge 5 3, Edge 3 1, Edge 2 5, Edge 5 1]
capacities1 = (Map.fromList [(Edge 1 2,4), (Edge 1 3,5), (Edge 3 2,3), (Edge 3 5,6), (Edge 4 5,2), (Edge 5 2,3), 
                        (Edge 5 3,6), (Edge 3 1,2), (Edge 2 5,4), (Edge 5 1, 6)])

graph1 = (Graph 5 8 1 5 edges capacities
                                
    )

path11 = [Edge 1 2, Edge 2 5, Edge 5 3, Edge 3 2]
path12 = [Edge 4 5, Edge 5 1, Edge 1 3]

testPathFlow = TestCase (assertEqual "Fix getting path flow." 3 (getPathFlow graph1 path11))
testPathFlow2 = TestCase (assertEqual "Fix getting path flow." 2 (getPathFlow graph1 path12))

testIncreaseFlow = TestCase (assertEqual "Fix increasing path flow." 5)


tests = TestList [
        TestLabel "Testing correct path flow." testPathFlow,
        TestLabel "Testing correct path flow." testPathFlow2
    ]
