module Lib where

import Datatypes
import qualified Data.Map as Map

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Some helper functions for getting information from the Graph

-- |Checks for the existence of a certain Vertex
vertexExist :: Int -> Graph -> Bool
vertexExist state gr = Map.member state gr

-- |Returns the amount of the vertices in the Graph
verticesAmount :: Graph -> Int
verticesAmount = Map.size

-- |Gets a list of all the adjacent vertices with weights if they exist otherwise Nothing
getAdjacentVertices :: Vertex -> Graph -> Maybe [Edge]
getAdjacentVertices state map = Map.lookup state map

-- |Gets a list of all the existing Vertices
getAllVertices :: Graph -> [Vertex]
getAllVertices = Map.keys

--- Some methods for printing info about the Graph to the screen

-- |Prints for a certain Vertex its edges with other vertices and weights to the screen
printAdjacentVertices :: Vertex -> Graph -> IO ()
printAdjacentVertices state map =
    case getAdjacentVertices state map of
        Nothing -> putStrLn "The Vertex doesn't exist"
        Just [] -> putStrLn "The Vertex doesn't have any adjacent Vertices"
        Just all@(x:xs) -> mapM_ putStrLn $ printConnection state all

-- |Puts info about the relation between a vertex and its edges in a list of Strings
printConnection :: Vertex -> [Edge] -> [String]
printConnection state connections = map (\(a,b) -> "From Vertex " ++ show state ++ " to Vertex " ++ show a ++ " with weight: " ++ show b) connections



-- earlier drafts

-- |Checks for the existence of a certain Vertex
-- vertexExist :: Graph -> Int -> Bool
-- vertexExist gr state = state `elem` gr

-- a :: Graph
-- a = [0,1,2,3,4]
