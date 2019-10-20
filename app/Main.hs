module Main where

import qualified Lib as L
import qualified Data.Map as Map
import Datatypes

main :: IO ()
main = L.someFunc

main' :: IO ()
main' = do
  putStrLn "What's the initial State?"
  firstState <- getLine
  putStrLn "What's the end State?"
  endState <- getLine
  return ()
