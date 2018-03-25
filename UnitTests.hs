module UnitTests where

import Test.HUnit
import Data.Map as Map
import GraphData

main :: IO Counts
main = runTestTT flowTests >> runTestTT residualTests >> runTestTT graphFlowTests

----------------------- CAPACITY AND FLOW TESTS --------------------

flowTests :: Test
flowTests = TestList [
        TestLabel "Testing getting path capacity." testPathCapacity,
        TestLabel "Testing getting path capacity." testPathCapacity2,
        TestLabel "Testing increasing flow." testIncreaseFlow
    ]
    where 
        testPathCapacity = TestCase (assertEqual "Fix getting path capacity." 3 (getPathCapacity graph1 path11))
        testPathCapacity2 = TestCase (assertEqual "Fix getting path capacity." 2 (getPathCapacity graph1 path12))
        testIncreaseFlow = TestCase $ assertEqual "Fix increasing path flow." 2 (getPathFlow  (increaseFlow flow1 path12 2) path12 )


graph1 :: TGraph
graph1 = (Graph 5 8 1 5 edges1 capacities1)
    where 
        capacities1 = (Map.fromList [(Edge 1 2,4), (Edge 1 3,5), (Edge 3 2,3), (Edge 3 5,6), (Edge 4 5,2), (Edge 5 2,3), 
                        (Edge 5 3,6), (Edge 3 1,2), (Edge 2 5,4), (Edge 5 1, 6)])
        edges1 = [Edge 1 2, Edge 1 3, Edge 3 2, Edge 3 5, Edge 4 5, Edge 4 2, Edge 5 3, Edge 3 1, Edge 2 5, Edge 5 1]

flow1 :: Map TEdge Word
flow1 = (Map.fromList [(Edge 1 2,0), (Edge 1 3,0), (Edge 3 2,0), (Edge 3 5,0), (Edge 4 5,0), (Edge 5 2,0), 
                        (Edge 5 3,0), (Edge 3 1,0), (Edge 2 5,0), (Edge 5 1, 0)])
path11 :: [TEdge]
path11 = [Edge 1 2, Edge 2 5, Edge 5 3, Edge 3 2]
path12 :: [TEdge]
path12 = [Edge 4 5, Edge 5 1, Edge 1 3]

---------------------- RESIDUAL GRAPH TESTS -----------------------

residualTests :: Test
residualTests = TestList [
        TestLabel "Testing creating residual graph." testResidual1,
        TestLabel "Testing creating residual graph." testResidual2,
        TestLabel "Testing creating residual graph." testResidual3
    ]

-- Input
graphRes1 :: TGraph
graphRes1 = (Graph 2 2 1 2 edgesRes1 capacitiesRes1)
    where
        edgesRes1 = [Edge 1 2, Edge 2 1]
        capacitiesRes1 = (Map.fromList [(Edge 1 2,4), (Edge 2 1,3)])

pathRes1 :: [TEdge]
pathRes1 = [Edge 1 2]
flowRes1 :: Word
flowRes1 = 2

-- Expected result
graphRes1Result :: TGraph
graphRes1Result = (Graph 2 2 1 2 edgesRes1 capacitiesRes1Result)
    where
        capacitiesRes1Result = (Map.fromList [(Edge 1 2,2), (Edge 2 1,5)])
        edgesRes1 = Map.keys capacitiesRes1Result

testResidual1 :: Test
testResidual1 = TestCase (assertEqual "Fix creating residual graph." graphRes1Result (createResidualGraph graphRes1 flowRes1 pathRes1))
 
----- RESIDUAL TEST 2

-- Input
graphRes2 :: TGraph
graphRes2 = (Graph 4 6 1 4 edgesRes2 capacitiesRes2)
    where
        edgesRes2 = [Edge 1 2, Edge 1 3, Edge 3 2, Edge 2 3, Edge 2 4, Edge 3 4]
        capacitiesRes2 = (Map.fromList [(Edge 1 2,4), (Edge 1 3,5), (Edge 3 2,3), (Edge 2 3,6),
            (Edge 2 4,4), (Edge 3 4,5)])
pathRes2 :: [TEdge]
pathRes2 = [Edge 1 3, Edge 3 2, Edge 2 4]
flowRes2 :: Word
flowRes2 = 3

-- Expected result
graphRes2Result :: TGraph
graphRes2Result = (Graph 4 7 1 4 edgesRes2Result capacitiesRes2Result)
    where 
        capacitiesRes2Result = (Map.fromList [(Edge 1 2,4),(Edge 1 3,2),(Edge 3 1,3),(Edge 2 3,9),
                (Edge 2 4,1),(Edge 4 2,3), (Edge 3 4,5)])
        edgesRes2Result = Map.keys capacitiesRes2Result

testResidual2 :: Test
testResidual2 = TestCase (assertEqual "Fix creating residual graph." graphRes2Result (createResidualGraph graphRes2 flowRes2 pathRes2))

----- RESIDUAL TEST 3


testResidual3 :: Test
testResidual3 = TestCase (assertEqual "Fix creating residual graph." graphRes3Result (createResidualGraph graphRes3 flowRes3 pathRes3))
    where
        -- Input
        graphRes3 = (Graph 2 1 1 2 edgesRes3 capacitiesRes3)
            where
                edgesRes3 = [Edge 1 2]
                capacitiesRes3 = (Map.fromList [(Edge 1 2,4)])

        pathRes3 = [Edge 1 2]
        flowRes3 = 4

        -- Expected result
        graphRes3Result :: TGraph
        graphRes3Result = (Graph 2 1 1 2 edgesRes3Result capacitiesRes3Result)
            where
                edgesRes3Result = Map.keys capacitiesRes3Result
                capacitiesRes3Result = (Map.fromList [(Edge 2 1,4)])

---------------------- MAX FLOW TESTS -----------------------

graphFlowTests :: Test
graphFlowTests = TestList[
        TestLabel "Testing creating flow graph." testGraphFlow1,
        TestLabel "Testing creating flow graph." testGraphFlow2
    ]

-- Input
graphFlow1 :: TGraph
graphFlow1 = (Graph 4 6 1 4 edgesFlow1 capacitiesFlow1)
    where
        edgesFlow1 = [Edge 1 2, Edge 1 3, Edge 3 2, Edge 2 3, Edge 2 4, Edge 3 4]
        capacitiesFlow1 = (Map.fromList [(Edge 1 2,4), (Edge 1 3,5), (Edge 3 2,3), (Edge 2 3,6),
            (Edge 2 4,4), (Edge 3 4,5)])

-- Expected result
resultFlow1 :: (Map TEdge Word, [[TEdge]])
resultFlow1 = (flowMap1, pathsFlow1)
    where
        flowMap1 = (Map.fromList [(Edge 1 2,4), (Edge 2 4,4), (Edge 1 3,5), (Edge 3 4, 5)])
        pathsFlow1 = [[Edge 1 3, Edge 3 4], [Edge 1 2, Edge 2 4]]


initFlow1 :: Map TEdge Word
initFlow1 = Map.fromList $ Prelude.map (\e -> (e, 0)) edgesFlow1
    where
        edgesFlow1 = [Edge 1 2, Edge 1 3, Edge 3 2, Edge 2 3, Edge 2 4, Edge 3 4]

testGraphFlow1 :: Test
testGraphFlow1 = TestCase (assertEqual "Fix computing max flow." resultFlow1 (findMaxFlow graphFlow1 [] initFlow1))

----- FLOW TEST 2

-- Input
graphFlow2 :: TGraph
graphFlow2 = Graph 3 2 1 3 edgesFlow2 capacitiesFlow2
    where
        edgesFlow2 = [Edge 1 2, Edge 2 3]
        capacitiesFlow2 = (Map.fromList [(Edge 1 2, 2), (Edge 2 3, 3)])

-- Expected result
resultFlow2 :: (Map TEdge Word, [[TEdge]])
resultFlow2 = (flowMap2, pathsFlow2)
    where
        flowMap2 = (Map.fromList [(Edge 1 2, 2),(Edge 2 3, 2)])
        pathsFlow2 = [[Edge 1 2, Edge 2 3]]


initFlow2 :: Map TEdge Word
initFlow2 = Map.fromList $ Prelude.map (\e -> (e, 0)) $ edges graphFlow2

testGraphFlow2 :: Test
testGraphFlow2 = TestCase (assertEqual "Fix computing max flow." resultFlow2 (findMaxFlow graphFlow2 [] initFlow2))
