module Main (
  main
, infoVertexMain
, shPathMain
) where

import qualified Lib as L
import qualified Data.Map as Map
import Datatypes

-- |Runs the text based GUI and shows the current options to choose from
main :: IO ()
main = do
  putStrLn "\nPlease choose an option:"
  mapM_ putStrLn $ tupleToString choices
  userChoice <- getLine
  let chosenGraph = newGraph
  case read userChoice :: Int of
    1 -> putStrLn $ show $ L.verticesAmount chosenGraph   -- gives the amount of existing Vertices
    2 -> putStrLn $ show $ L.getAllVertices chosenGraph   -- gives all the existing Vertices
    3 -> infoVertexMain chosenGraph                       -- gives all the connected Edges from a certain Vertex
    4 -> shPathMain chosenGraph                           -- calculates the shortest path
    5 -> return ()                                        -- quits the program

-- |Gives back all the connected Edges of a certain Vertex
infoVertexMain :: Graph -> IO ()
infoVertexMain gr = do
  putStrLn "Which vertex?"
  fState <- getLine
  let firstState = read fState :: Int
  mapM_ putStrLn $ L.adjacentVerticesToString firstState gr
  main

-- |Calculates the shortest path between 2 Vertices
shPathMain :: Graph -> IO ()
shPathMain gr = do
  putStrLn "\nWhat's the initial State?"
  fState <- getLine
  let firstState = read fState :: Int
  if not $ L.vertexExist firstState gr
    then do
      putStrLn "The State doesn't exist in the Graph"
      shPathMain gr
    else do
      putStrLn "What's the end State?"
      secState <- getLine
      let secondState = read secState :: Int
      if not $ L.vertexExist secondState gr
        then do
          putStrLn "The State doesn't exist in the Graph"
          shPathMain gr
        else do
          putStrLn "Calculating the shortest path........."
          main


-- |Sets an array of tuples to a array of Strings
tupleToString :: [(Int, String)] -> [String]
tupleToString list = map (\(a,b) -> show a ++ " - " ++ b) list

-- |All the current choices the user can choose from
choices :: [(Int, String)]
choices = [(1, "Get the number of vertices in the Graph")
          ,(2, "Get all vertices")
          ,(3, "Get information about a specific vertex")
          ,(4, "Get the shortest path")
          ,(5, "Quit")
          ]


-- testing
newGraph :: Graph
newGraph = Map.fromList
    [(0, [(1, 5), (2, 2)])
    ,(1, [(0, 5), (3, 3)])
    ,(2, [(0, 2), (3, 1), (4, 7)])
    ,(3, [(1, 3), (2, 1), (4, 2)])
    ,(4, [(2, 7), (3, 2)])
    ]
