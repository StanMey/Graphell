module Main where

import qualified Lib as L
import qualified Data.Map as Map
import Datatypes

-- |
main :: IO ()
main = do
  putStrLn "\nPlease choose an option:"
  userChoice <- getLine
  let chosenGraph = newGraph
  case read userChoice :: Int of
    1 -> putStrLn $ show $ L.verticesAmount chosenGraph
    2 -> putStrLn $ show $ L.getAllVertices chosenGraph
    3 -> infoVertexMain chosenGraph
    4 -> shPathMain chosenGraph
    5 -> return ()


-- |
infoVertexMain :: Graph -> IO ()
infoVertexMain gr = do
  putStrLn "Which vertex?"
  fState <- getLine
  let firstState = read fState :: Int
  L.printAdjacentVertices firstState gr
  main

-- |
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


-- |
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
