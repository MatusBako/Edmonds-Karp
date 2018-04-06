module Dimacs where

import GraphData
import System.IO
import Text.Read
import qualified Data.Map as Map

data TInputData = InputData {
    inputGraph :: TGraph,
    inputHandle :: Handle
}

-- Open load graph from argument or stdin and create graph.
parseInput :: String -> IO TInputData
parseInput filePath = do 
    handle <- if filePath == "" 
        then return stdin
        else openFile filePath ReadMode

    contents <- hGetContents handle 

    let fileLines = filter 
            (\x -> length x > 0 && length ((words x)!!0) == 1 &&  x!!0 `elem` "pna") 
            (lines contents)

    let graph = foldl parseLine ((\x -> return x) $ InputData (Graph 0 0 0 0 [] Map.empty) handle) fileLines
    verifyIOInput $ graph


verifyIOInput :: IO TInputData -> IO TInputData
verifyIOInput inputData  = inputData >>= (\(InputData graph handle) -> verifyInput (InputData graph handle))

verifyInput :: TInputData -> IO TInputData
verifyInput (InputData graph handle)
    | (fromIntegral $ (edgeCount graph)::Word) /= (fromIntegral $ (length $ edges graph)::Word) 
        = error "Edge count is different from actual number of edges."
    | source graph == 0 = error "Source node not given."
    | target graph == 0 = error "Source node not given."
    | otherwise     = return (InputData graph handle)

exitParsing :: Handle -> String -> IO TInputData
exitParsing handle errorMsg = hClose handle >>  error errorMsg

-- Initial error checking on each line.
parseLine :: IO TInputData -> String -> IO TInputData
parseLine inputData ""   = inputData
parseLine inputData line = inputData >>= (\(InputData graph handle) -> if 
    length firstWord  /= 1 then  exitParsing handle "Line should start with single letter." 
    else if (head firstWord) `notElem` "cpna" then exitParsing handle ("Line can't start with letter \"" ++ firstWord ++ "\".")
    else parseCorrectLine (InputData graph handle) (head firstWord) line)
    where
        firstWord = (words  line)!!0

-- Parse each line type.
parseCorrectLine :: TInputData -> Char -> String -> IO TInputData
parseCorrectLine inputData 'c' _ = return inputData
parseCorrectLine (InputData (Graph _ _ src tar edgs cap) handle) 'p' line 
    | desc /= "max"                 = exitParsing handle "Second argument of line \"p\" is not \"max\"."
    | (length $ words line) /= 4    = exitParsing handle "Wrong line format, use following: n node s/t."
    | otherwise                     = case nStr of 
        Nothing -> exitParsing handle "Error occured while parsing edge count."
        Just n  -> case eStr of 
            Nothing -> exitParsing handle "Error occured while parsing edge count."
            Just e  -> return (InputData (Graph n e src tar edgs cap) handle) 
        where 
            desc = words line!!1
            nStr = readMaybe $ words line!!2 :: Maybe Word
            eStr = readMaybe $ words line!!3 :: Maybe Word

parseCorrectLine (InputData (Graph nCnt  eCnt src tar edgs cap) handle) 'n' line 
    | (length $ words line) /= 3        = exitParsing handle "Wrong line format, use following: n node s/t."
    | (length $ (words line)!!2) /= 1   = exitParsing handle "Mark the node as source or target (s/t)."
    | otherwise                         = case idStr of 
        Nothing -> exitParsing handle "Error occured while parsing source/target id."
        Just nodeId -> addSrcDest (InputData (Graph nCnt  eCnt src tar edgs cap) handle) side nodeId
        where 
            idStr = readMaybe $ words line!!1 :: Maybe Word
            side =  head $ words line!!2

parseCorrectLine (InputData (Graph nCnt  eCnt src tar edgs cap) handle) 'a' line 
    | (length $ words line) /= 4    = exitParsing handle "Wrong line format, use following: a node1 node2."
    | otherwise                     = case id1Str of
        Nothing -> exitParsing handle "Error while parsing forst id of edge."
        Just id1-> case id2Str of 
            Nothing -> exitParsing handle "Error while parsing second id of edge."
            Just id2 -> case capStr of 
                Nothing -> exitParsing handle "Error while parsing capacity of edge."
                Just newCapacity -> addEdge (InputData (Graph nCnt  eCnt src tar edgs cap) handle) (Edge id1 id2) newCapacity
        where   
            id1Str = readMaybe $ words line!!1 :: Maybe Word
            id2Str = readMaybe $ words line!!2 :: Maybe Word
            capStr = readMaybe $ words line!!3 :: Maybe Word
parseCorrectLine (InputData _ handle) _ _
    = exitParsing handle "Wrong starting symbol."


addSrcDest :: TInputData -> Char -> Node -> IO TInputData
addSrcDest (InputData (Graph nCnt  _ _ _ _ _) handle) _ newNode 
    | newNode == 0 || newNode > nCnt = exitParsing handle "Wrong node id when defining source or target." 

addSrcDest (InputData (Graph nCnt  eCnt src tar edgs cap) handle) 's' nodeId
    | src /= 0      = exitParsing handle "Source can't be added more than once."
    | nodeId == tar = exitParsing handle "Source and target must be different."
    | nodeId == 0   = exitParsing handle "Source can't be 0."
    | nodeId > nCnt = exitParsing handle "Source can't be bigger than node count."
    | otherwise     = return (InputData (Graph nCnt  eCnt nodeId tar edgs cap) handle)

addSrcDest (InputData (Graph nCnt  eCnt src tar edgs cap) handle) 't' nodeId
    | tar /= 0      = exitParsing handle "Target can't be added more than once."
    | nodeId == src = exitParsing handle "Target and target must be different."
    | nodeId == 0   = exitParsing handle "Target can't be 0."
    | nodeId > nCnt = exitParsing handle "Target can't be bigger than node count."
    | otherwise     = return (InputData (Graph nCnt  eCnt src nodeId edgs cap) handle)

addSrcDest (InputData _ handle) _ _ = 
    exitParsing handle "Choose source or target (s/t)."


addEdge :: TInputData -> TEdge -> Word -> IO TInputData
addEdge (InputData (Graph nCnt _ _ _ _ _) handle) (Edge id1 id2) _
    | id1 == id2                = exitParsing handle "Selfloops are not allowed."
    | id1 == 0 || id1 > nCnt    = exitParsing handle "Wrong node id when defining edge." 
    | id2 == 0 || id2 > nCnt    = exitParsing handle "Wrong node id when defining edge." 

addEdge (InputData (Graph nCnt eCnt src tar edgs cap) handle) newEdge newCapacity
    | newEdge `elem` edgs      = exitParsing handle "Edge can't be added more than once."
    | otherwise                 = return (InputData (Graph nCnt eCnt src tar (newEdge:edgs) (Map.insert newEdge newCapacity cap)) handle)
