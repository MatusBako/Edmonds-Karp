module GraphTests where

import Test.HUnit
import Data.Map as Map
import GraphData
import Debug.Trace

----------------------- CAPACITY AND FLOW TESTS --------------------

edges1 = [Edge 1 2, Edge 1 3, Edge 3 2, Edge 3 5, Edge 4 5, Edge 4 2, Edge 5 3, Edge 3 1, Edge 2 5, Edge 5 1]
capacities1 = (Map.fromList [(Edge 1 2,4), (Edge 1 3,5), (Edge 3 2,3), (Edge 3 5,6), (Edge 4 5,2), (Edge 5 2,3), 
                        (Edge 5 3,6), (Edge 3 1,2), (Edge 2 5,4), (Edge 5 1, 6)])

flow1 = (Map.fromList [(Edge 1 2,0), (Edge 1 3,0), (Edge 3 2,0), (Edge 3 5,0), (Edge 4 5,0), (Edge 5 2,0), 
                        (Edge 5 3,0), (Edge 3 1,0), (Edge 2 5,0), (Edge 5 1, 0)])

graph1 = (Graph 5 8 1 5 edges1 capacities1)

path11 = [Edge 1 2, Edge 2 5, Edge 5 3, Edge 3 2]
path12 = [Edge 4 5, Edge 5 1, Edge 1 3]


flowTests = TestList [
        TestLabel "Testing getting path capacity." testPathCapacity,
        TestLabel "Testing getting path capacity." testPathCapacity2,
        TestLabel "Testing increasing flow." testIncreaseFlow
    ]
    where 
        testPathCapacity = TestCase (assertEqual "Fix getting path capacity." 3 (getPathCapacity graph1 path11))
        testPathCapacity2 = TestCase (assertEqual "Fix getting path capacity." 2 (getPathCapacity graph1 path12))
        testIncreaseFlow = TestCase $ assertEqual "Fix increasing path flow." 2 (getPathFlow  (increaseFlow flow1 path12 2 )path12 )

---------------------- RESIDUAL GRAPH TESTS -----------------------

edgesRes1 = [Edge 1 2, Edge 2 1]
capacitiesRes1 = (Map.fromList [(Edge 1 2,4), (Edge 2 1,3)])
graphRes1 = (Graph 2 2 1 2 edgesRes1 capacitiesRes1)
pathRes1 = [Edge 1 2]
flowRes1 = 2

capacitiesRes1Result = (Map.fromList [(Edge 1 2,2), (Edge 2 1,5)])
graphRes1Result = (Graph 2 2 1 2 edgesRes1 capacitiesRes1Result)

testResidual1 = TestCase (assertEqual "Fix creating residual graph." graphRes1Result (createResidualGraph graphRes1 flowRes1 pathRes1))
 
-----

edgesRes2 = [Edge 1 2, Edge 1 3, Edge 3 2, Edge 2 3, Edge 2 4, Edge 3 4]
capacitiesRes2 = (Map.fromList [(Edge 1 2,4), (Edge 1 3,5), (Edge 3 2,3), (Edge 2 3,6),
    (Edge 2 4,4), (Edge 3 4,5)])
graphRes2 = (Graph 4 6 1 4 edgesRes2 capacitiesRes2)
pathRes2 = [Edge 1 3, Edge 3 2, Edge 2 4]
flowRes2 = 3

capacitiesRes2Result = (Map.fromList [(Edge 1 2,4),(Edge 1 3,2),(Edge 3 1,3),(Edge 2 3,9),
        (Edge 2 4,1),(Edge 4 2,3), (Edge 3 4,5)])
edgesRes2Result = Map.keys capacitiesRes2Result
graphRes2Result = (Graph 4 7 1 4 edgesRes2Result capacitiesRes2Result) -- FIX EDGE COUNT

testResidual2 = TestCase (assertEqual "Fix creating residual graph." graphRes2Result (createResidualGraph graphRes2 flowRes2 pathRes2))

-----

edgesRes3 = [Edge 1 2]
capacitiesRes3 = (Map.fromList [(Edge 1 2,4)])
graphRes3 = (Graph 2 1 1 2 edgesRes3 capacitiesRes3)
pathRes3 = [Edge 1 2]
flowRes3 = 4

capacitiesRes3Result = (Map.fromList [(Edge 2 1,4)])
graphRes3Result = (Graph 2 1 1 2 (Map.keys capacitiesRes3Result) capacitiesRes3Result)

testResidual3 = TestCase (assertEqual "Fix creating residual graph." graphRes3Result (createResidualGraph graphRes3 flowRes3 pathRes3))
 
main = createResidualGraph graphRes3 flowRes3 pathRes3

residualTests = TestList [
        TestLabel "Testing creating residual graph." testResidual1,
        TestLabel "Testing creating residual graph." testResidual2,
        TestLabel "Testing creating residual graph." testResidual3
    ]

---------------------- MAX FLOW TESTS -----------------------

graphFlowTests = TestList[
        TestLabel "Testing creating flow graph." testGraphFlow1,
        TestLabel "Testing creating flow graph." testGraphFlow2
    ]

edgesFlow1 = [Edge 1 2, Edge 1 3, Edge 3 2, Edge 2 3, Edge 2 4, Edge 3 4]
capacitiesFlow1 = (Map.fromList [(Edge 1 2,4), (Edge 1 3,5), (Edge 3 2,3), (Edge 2 3,6),
    (Edge 2 4,4), (Edge 3 4,5)])
graphFlow1 = (Graph 4 6 1 4 edgesRes2 capacitiesRes2)

flowMap1 = (Map.fromList [(Edge 1 2,4), (Edge 2 4,4), (Edge 1 3,5), (Edge 3 4, 5)])
pathsFlow1 = [[Edge 1 3, Edge 3 4], [Edge 1 2, Edge 2 4]]
resultFlow1 = (flowMap1, pathsFlow1)

--findMaxFlow :: TGraph -> [Path] -> Map.Map TEdge Word -> (Map.Map TEdge Word, [Path])
initFlow1 = Map.fromList $ Prelude.map (\e -> (e, 0)) edgesFlow1

--main = findMaxFlow graphFlow1 [] initFlow1

testGraphFlow1 = TestCase (assertEqual "Fix computing max flow." resultFlow1 (findMaxFlow graphFlow1 [] initFlow1))

-----

edgesFlow2 = [Edge 1 2, Edge 2 3]
capacitiesFlow2 = (Map.fromList [(Edge 1 2, 2), (Edge 2 3, 3)])
graphFlow2 = Graph 3 2 1 3 edgesFlow2 capacitiesFlow2

flowMap2 = (Map.fromList [(Edge 1 2, 2),(Edge 2 3, 2)])
pathsFlow2 = [[Edge 1 2, Edge 2 3]]
resultFlow2 = (flowMap2, pathsFlow2)

initFlow2 = Map.fromList $ Prelude.map (\e -> (e, 0)) edgesFlow2

testGraphFlow2 = TestCase (assertEqual "Fix computing max flow." resultFlow2 (findMaxFlow graphFlow2 [] initFlow2))
