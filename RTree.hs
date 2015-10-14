module RoseTree where

import Data.Maybe
import Data.List

type Node = String
type Depth = Int

type IncPair = (Node, [Node])
data Graph =
  Graph [IncPair]
  deriving (Show,Eq)

data RoseTree a =
  RoseTree a [RoseTree a]
  deriving (Show,Eq)
type WordTree = RoseTree (Node, Depth)

incidents :: Node -> Graph -> Maybe [Node]
incidents node (Graph incs) = lookup node incs

-- | Print a tree of descendants of a node.
descTree' :: Node -- the node to look for
          -> Graph -- the graph to look in
          -> [Node] -- ancestors to exclude
          -> Depth -- the depth of the current node
          -> WordTree -- the tree of descendants

descTree' node graph ancs depth =
  RoseTree (node,depth + 1)
           (map (\n ->
                   descTree' n
                             graph
                             (node : ancs)
                             (depth + 1))
                 nonAncestorIncidents)
    where allIncidents = fromJust $ incidents node graph
          nonAncestorIncidents = allIncidents \\ ancs

descTree :: Node -> Graph -> WordTree
descTree node graph = descTree' node graph [] 0

maxDepth :: WordTree -> Depth
maxDepth (RoseTree (_,depth) kids) =
  case kids of
    [] -> depth
    _  -> maximum $ depth : map maxDepth kids

longestSequenceFrom :: Node -> Graph -> Depth
longestSequenceFrom n g = maxDepth $ descTree n g

sampleGraph :: Graph
sampleGraph =
  Graph [
    (a,[b])
   ,(b,[a,c,d,e])
   ,(c,[b])
   ,(d,[b])
   ,(e,[b])
   ]
  where [a,b,c,d,e] = map (: []) "abcde"

loopedGraph :: Graph
loopedGraph =
  Graph [
    (a,[b])
   ,(b,[a,c,d,e])
   ,(c,[b,d])
   ,(d,[b,c])
   ,(e,[b])
   ]
  where [a,b,c,d,e] = map (: []) "abcde"
