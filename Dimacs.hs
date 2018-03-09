module Dimacs where

    import GraphData
    import System.IO
    import Data.List
    import Text.Read
    import qualified Data.Map as Map
    import Debug.Trace

    data TInputData = InputData {
        graph :: TGraph,
        handle :: Handle
    }

    parseInput :: String -> IO TInputData
    parseInput filePath = do
        handle <- openFile filePath ReadMode
        contents <- hGetContents handle
        let l = filter 
                (\x -> length x > 0 && length ((words x)!!0) == 1 &&  x!!0 `elem` "pna") 
                (lines contents)

        let graph = foldl parseLine ((\x -> return x) $ InputData (Graph 0 0 0 0 [] Map.empty) handle) l
        verifyInput $ graph

    verifyInput :: IO TInputData -> IO TInputData
    verifyInput inputData  = inputData >>= (\(InputData graph handle) -> if 
         (fromIntegral $ (edgeCount graph)::Word) /= (fromIntegral $ (length $ edges graph)::Word) then inputData else error "Edge count is different from actual number of edges.")

    exitParsing :: Handle -> String -> IO TInputData
    exitParsing handle errorMsg = hClose handle >>  error errorMsg

    parseLine :: IO TInputData -> String -> IO TInputData
    parseLine inputData ""   = inputData
    parseLine inputData line = inputData >>= (\(InputData graph handle) -> if 
        length firstWord  /= 1 then  exitParsing handle "Line should start with single letter." 
        else if (head firstWord) `notElem` "cpna" then exitParsing handle ("Line can't start with letter \"" ++ firstWord ++ "\".")
        else parseCorrectLine (InputData graph handle) (head firstWord) line)
        where
            firstWord = (words  line)!!0


    parseCorrectLine :: TInputData -> Char -> String -> IO TInputData
    parseCorrectLine (InputData (Graph nCnt  eCnt src tar edges cap) handle) 'c' line = return (InputData (Graph nCnt  eCnt src tar edges cap) handle)
    parseCorrectLine (InputData (Graph nCnt  eCnt src tar edges cap) handle) 'p' line 
        | desc /= "max"                 = exitParsing handle "Second argument of line \"p\" is not \"max\"."
        | (length $ words line) /= 4    = exitParsing handle "Wrong line format, use following: n node s/t."
        | otherwise                     = case nStr of 
            Nothing -> exitParsing handle "Error occured while parsing edge count."
            Just n  -> case eStr of 
                Nothing -> exitParsing handle "Error occured while parsing edge count."
                Just e  -> return (InputData (Graph n e src tar edges cap) handle) 
            where 
                desc = words line!!1
                nStr = readMaybe $ words line!!2 :: Maybe Word
                eStr = readMaybe $ words line!!3 :: Maybe Word

    parseCorrectLine (InputData (Graph nCnt  eCnt src tar edges cap) handle) 'n' line 
        | (length $ words line) /= 3    = exitParsing handle "Wrong line format, use following: n node s/t."
        | otherwise                     = case idStr of 
            Nothing -> exitParsing handle "Error occured while parsing source/target id."
            Just id -> if id == 0 || id > nCnt 
                then exitParsing handle "Wrong node id when defining edge." 
                else case  side of 
                    "s" -> return (InputData (Graph nCnt  eCnt id tar edges cap) handle)
                    "t" -> return (InputData (Graph nCnt  eCnt src id edges cap) handle)
                    _   -> exitParsing handle "Choose source or target (s/t)."
            where 
                idStr = readMaybe $ words line!!1 :: Maybe Word
                side = words line!!2
    
    parseCorrectLine (InputData (Graph nCnt  eCnt src tar edges cap) handle) 'a' line 
        | (length $ words line) /= 4    = exitParsing handle "Wrong line format, use following: a node1 node2."
        | otherwise                     = case id1Str of
            Nothing -> exitParsing handle "Error while parsing forst id of edge."
            Just id1-> case id2Str of 
                Nothing -> exitParsing handle "Error while parsing second id of edge."
                Just id2 -> case capStr of 
                    Nothing -> exitParsing handle "Error while parsing capacity of edge."
                    Just capacity -> return (InputData (Graph nCnt  (eCnt+1) src tar (newEdge:edges) (Map.insert newEdge capacity cap)) handle)
                    where 
                        newEdge = Edge id1 id2
            where   
                id1Str = readMaybe $ words line!!1 :: Maybe Word
                id2Str = readMaybe $ words line!!2 :: Maybe Word
                capStr = readMaybe $ words line!!3 :: Maybe Word
    parseCorrectLine (InputData (Graph nCnt  eCnt src tar edges cap) handle) _ line = exitParsing handle "Wrong starting symbol."
