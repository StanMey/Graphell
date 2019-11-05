module Lib where

import Datatypes
import qualified Data.Map as Map


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


-- Functions to get all the information about the Edges
-- |Get the total amount of Edges in the undirectional Graph
getEdgesAmount :: Graph -> Int
getEdgesAmount gr = (sum $ [countEdges x gr | x <- (getAllVertices gr)]) `div` 2

-- |Counts the connected Edges of a certain Vertex
countEdges :: Vertex -> Graph -> Int
countEdges v gr = case getAdjacentVertices v gr of
  Nothing -> 0
  Just [] -> 0
  Just all@(x:xs) -> length all

-- |Get the total sum of all the Weights of the Edges from one Vertex
getWeightSum :: Vertex -> Graph -> Int
getWeightSum state gr = sum $ map (\(a,b) -> b) $ getAllEdges state gr

getAllEdges :: Vertex -> Graph -> [Edge]
getAllEdges v gr = case getAdjacentVertices v gr of
  Nothing -> [(0,0)]
  Just [] -> [(0,0)]
  Just all@(x:xs) -> all

-- |Returns for a certain Vertex its edges with other vertices and weights
adjacentVerticesToString :: Vertex -> Graph -> [String]
adjacentVerticesToString state map =
    case getAdjacentVertices state map of
        Nothing -> ["The Vertex doesn't exist"]
        Just [] -> ["The Vertex doesn't have any adjacent Vertices"]
        Just all@(x:xs) -> edgeToString state all

-- |Puts info about the relation between a vertex and its edges in a list of Strings
edgeToString :: Vertex -> [Edge] -> [String]
edgeToString state connections = map (\(a,b) -> "From Vertex " ++ show state ++ " to Vertex " ++ show a ++ " with weight: " ++ show b) connections



-- earlier drafts

-- |Checks for the existence of a certain Vertex
-- vertexExist :: Graph -> Int -> Bool
-- vertexExist gr state = state `elem` gr

-- a :: Graph
-- a = [0,1,2,3,4]
