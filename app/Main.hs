module Main (
  main
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
    1 -> putStrLn $ show $ L.verticesAmount chosenGraph     -- gives the amount of existing Vertices
    2 -> putStrLn $ show $ L.getAllVertices chosenGraph     -- gives all the existing Vertices
    3 -> infoVertexMain chosenGraph 3                       -- gives all the connected Edges from a certain Vertex
    4 -> putStrLn $ show $ L.getEdgesAmount chosenGraph     -- gives the amount of existing Edges
    5 -> infoVertexMain chosenGraph 5                       -- gives the total amount of the weights of all the connected Edges
    6 -> infoVerticesMain chosenGraph 6                     -- Checks if two Vertices are connected
    7 -> infoVerticesMain chosenGraph 7                     -- Checks if two Vertices share a common Vertex
    8 -> infoVerticesMain chosenGraph 8                     -- calculates the shortest path
    9 -> return ()                                          -- quits the program


-- |Gives back info from a certain Vertex based on the users choice
infoVertexMain :: Graph -> Int -> IO ()
infoVertexMain gr choice = do
  putStrLn "Which vertex?"
  fState <- getLine
  let chState = read fState :: Int
  if not $ L.vertexExist chState gr
    then do
      putStrLn "The State doens't exist in the Graph"
      infoVertexMain gr choice
    else do
      case choice of
        3 -> mapM_ putStrLn $ L.adjacentVerticesToString chState gr
        5 -> let vInfo = show $ L.getWeightSum chState gr
             in putStrLn $ "All the connected Edges together have a weight of: " ++ vInfo
      main

-- |Gives back som info about the relation of two Vertices based on the users choice
infoVerticesMain :: Graph -> Int -> IO ()
infoVerticesMain gr choice = do
  putStrLn "\nWhat's the initial State?"
  fState <- getLine
  let firstState = read fState :: Int
  if not $ L.vertexExist firstState gr
    then do
      putStrLn "The State doesn't exist in the Graph"
      infoVerticesMain gr choice
    else do
      putStrLn "What's the end State?"
      secState <- getLine
      let secondState = read secState :: Int
      if not $ L.vertexExist secondState gr
        then do
          putStrLn "The State doesn't exist in the Graph"
          infoVerticesMain gr choice
        else do
          case choice of
            6 -> putStrLn $ show $ L.twoVerticesConn firstState secondState gr
            7 -> putStrLn $ show $ L.getCommonVertices firstState secondState gr
            8 -> putStrLn "Calculating the shortest path........."
          main


-- |Sets an array of tuples to a array of Strings
tupleToString :: [(Int, String)] -> [String]
tupleToString list = map (\(a,b) -> show a ++ " - " ++ b) list

-- |All the current choices the user can choose from
choices :: [(Int, String)]
choices = [(1, "Get the number of Vertices in the Graph")
          ,(2, "Get all Vertices")
          ,(3, "Get information about a specific Vertex")
          ,(4, "Get the amount of existing Edges")
          ,(5, "Get the total sum of all weights from one Vertex")
          ,(6, "Check if two Vertices are connected")
          ,(7, "Check if two Vertices share common Vertex/Vertices")
          ,(8, "Get the shortest path")
          ,(9, "Quit")
          ]


-- testing
newGraph :: Graph
newGraph = Map.fromList
    [(0, [(1, 5), (2, 2)])
    ,(1, [(0, 5), (3, 3)])
    ,(2, [(0, 2), (3, 1), (4, 7)])
    ,(3, [(1, 3), (2, 1), (4, 2)])
    ,(4, [(2, 7), (3, 2)])
    ,(5, [])
    ]
