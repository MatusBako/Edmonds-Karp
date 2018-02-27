import Dimacs

import GraphData
import Text.Read
import System.IO    

findKey :: ( Eq k ) => k -> [( k , v )] -> Maybe v
findKey key [] = Nothing
findKey key (( k , v ): xs ) = 
    if key == k
    then 
        Just v
    else 
        findKey key xs



main = do
    --let phoneBook = [( "betty" ,"555-2938" ), ( " bonnie " ," 452 -2928 " ),( " patsy " ," 493 -2928 " ),( " lucille " ," 205 -2928 " ),( " wendy " ," 939 -8282 " ),( " penny " ," 853 -2492 " )]
    --print $ findKey "betty" phoneBook
    
    --let e = createEdges [[5,5],[2,5],[9,4]]
    --let e = Graph 3 [Edge 1 3, Edge 3 2]
    --let e = Graph 3 (createEdges [[1,3],[2,3],[3,1]])
    
    parseInput "in" >>= (\x -> (print $ graph x) >> (hClose $ handle x))
    --print $ graph data
    --hClose $ handle data
    --filter (\(x:xs) -> x /= 'c') ["c awdwa", "caar","p 1 2", "e 4 5"]



    {--let n = readMaybe $ words "awd 2" !! 0 :: Maybe Int
    case n of 
        Nothing -> print "chyba"
        Just k -> print k
    --}

