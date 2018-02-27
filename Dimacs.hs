module Dimacs where

    import GraphData
    import System.IO
    import Data.List
    import Text.Read
    import qualified Data.Map as Map

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

        let graph = foldl parseLine (Graph 0 0 0 0 [] Map.empty) l
        return $ InputData graph handle

    -- TODO: node ID can't be 0
    -- 0 node / edge count doesn't make sense
    -- given and actual node (edge) counts must be equal
    parseLine :: TGraph -> String -> TGraph
    parseLine (Graph nCnt  eCnt src tar edges cap) line = case line!!0 of
        'p' -> case nStr of 
            Nothing -> error "p nCnt"
            Just n  -> case eStr of 
                Nothing -> error "p eCnt"
                Just e  -> (Graph n e src tar edges cap)
            where nStr = readMaybe $ words line!!2 :: Maybe Word
                  eStr = readMaybe $ words line!!3 :: Maybe Word
        'n' -> case idStr of 
                Nothing -> error "n id"
                Just id -> case  side of 
                    "s" -> (Graph nCnt  eCnt id tar edges cap)
                    "t" -> (Graph nCnt  eCnt src id edges cap)
                    _   -> error "Choose source or target (s/t)."
            where idStr = readMaybe $ words line!!1 :: Maybe Word
                  side = words line!!2
        'a' -> case id1Str of
            Nothing -> error "a id1"
            Just id1-> case id2Str of 
                Nothing -> error "a id2"
                Just id2 -> case capStr of 
                    Nothing -> error "a capacity"
                    Just capacity -> (Graph nCnt  eCnt src tar (edges++[newEdge]) (Map.insert newEdge capacity cap))
                    where 
                        newEdge = Edge id1 id2
            where   id1Str = readMaybe $ words line!!1 :: Maybe Word
                    id2Str = readMaybe $ words line!!2 :: Maybe Word
                    capStr = readMaybe $ words line!!3 :: Maybe Word
