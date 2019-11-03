module Datatypes where

import qualified Data.Map as Map

type Vertex = Int
-- ^A single vertex is being noted as an integer

type Weight = Int
-- ^The weight a Edge has between two points in the graph

type Edge = (Vertex, Weight)
-- ^A edge holds a certain direction to another Vertex with a weight to it

type Graph = Map.Map Vertex [Edge]
-- ^A table is a map from one vertex to other vertices




-- earlier drafts

-- type Graph = [Vertex]
-- ^The complete list of all vertices
