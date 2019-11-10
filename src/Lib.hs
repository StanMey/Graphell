{-|

Module      : Lib
Description : Module containing all necessary functions to retrieve information from a Graph
Copyright   : (c) Stan Meyberg, 2019
License     : BSD3
Maintainer  : stan.meyberg@student.hu.nl

This module contains all necessary functions to retrieve information from a Graph.

-}

module Lib where

import Datatypes
import qualified Data.Map as Map


-- *Some helper functions for getting the basic information from the Graph
-- |Checks for the existence of a certain Vertex
vertexExist :: Vertex -> Graph -> Bool
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


-- *Functions to get information about the Edges
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

-- |Gets all the Edges from a certain Vertex
getAllEdges :: Vertex -> Graph -> [Edge]
getAllEdges v gr = case getAdjacentVertices v gr of
  Nothing -> []
  Just [] -> []
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


-- *Functions to get information about connections in the Graph
-- |Gets all the adjacent connected Edges and returns all the Vertices of those Edges
getOnlyConnVertices :: Vertex -> Graph -> [Vertex]
getOnlyConnVertices v gr = map (\(a,b) -> a) $ getAllEdges v gr

-- |Checks if two Vertices are connected
twoVerticesConn :: Vertex -> Vertex -> Graph -> Bool
twoVerticesConn v1 v2 gr = let a = getOnlyConnVertices v1 gr
                           in v2 `elem` a

-- |Returns any common connected Vertices between two Vertices
getCommonVertices :: Vertex -> Vertex -> Graph -> [Vertex]
getCommonVertices v1 v2 gr = let firstList = getOnlyConnVertices v1 gr
                                 secondList = getOnlyConnVertices v2 gr
                              in compareTwoLists firstList secondList

-- |Checks if two Vertices share a common connected Vertex
haveCommonVertices :: Vertex -> Vertex -> Graph -> Bool
haveCommonVertices v1 v2 gr = let vAmount = length $ getCommonVertices v1 v2 gr
                              in if vAmount > 0
                                  then True
                                  else False

-- |Compares two lists and returns the common Vertices
compareTwoLists :: [Vertex] -> [Vertex] -> [Vertex]
compareTwoLists [] [] = []
compareTwoLists (x:xs) [] = []
compareTwoLists [] (x:xs) = []
compareTwoLists (x:xs) all@(z:zs) = if x `elem` all
                                      then x : compareTwoLists xs all
                                      else compareTwoLists xs all
