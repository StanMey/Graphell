module Lib where

import Datatypes
import qualified Data.Map as Map

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- |Checks for the existence of a certain Vertex
vertexExist :: Graph -> Int -> Bool
vertexExist gr state = state `elem` gr

-- |Gets a list of all the adjacent vertices if they exist otherwise Nothing
getAdjacentVertices :: Vertex -> Table -> Maybe [Edge]
getAdjacentVertices state map = Map.lookup state map


--- Some methods for printing info about the Graph to the screen

-- |Prints for a certain Vertex its edges with other vertices and weights to the screen
printAdjacentVertices :: Vertex -> Table -> IO ()
printAdjacentVertices state map =
    case Map.lookup state map of
        Nothing -> putStrLn "The Vertex doesn't exist"
        Just [] -> putStrLn "The Vertex doesn't have any adjacent Vertices"
        Just all@(x:xs) -> mapM_ putStrLn $ printConnection state all

-- |Puts info about the relation between a vertex and its edges in a list of Strings
printConnection :: Vertex -> [Edge] -> [String]
printConnection state connections = map (\(a,b) -> "From Vertex " ++ show state ++ " to Vertex " ++ show a ++ " with weight: " ++ show b) connections


-- testing
a :: Graph
a = [0,1,2,3,4]

newGraph :: Table
newGraph = Map.fromList
    [(0, [(1, 5), (2, 2)])
    ,(1, [(0, 5), (3, 3)])
    ,(2, [(0, 2), (3, 1), (4, 7)])
    ,(3, [(1, 3), (2, 1), (4, 2)])
    ,(4, [(2, 7), (3, 2)])
    ]
