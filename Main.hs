module RTreeWordGame where

import Data.Maybe (fromJust)
import Data.List ((\\), groupBy, sortBy, foldl1', intercalate)
import Data.Function (on)
import Data.Ord (comparing)

-- Let's declare all our types first.
type Node = String
type Depth = Int
type Path = [Node]

type IncPair = (Node, [Node])
data Graph = Graph [IncPair] deriving (Show,Eq)

data RoseTree a = RoseTree a [RoseTree a] deriving (Show,Eq)

-- | Return all the nodes connected to the current node by an edge.
incidents :: Node -> Graph -> [Node]
incidents node (Graph incs) = fromJust $ lookup node incs

-- | Return all the nodes of the graph.
nodes :: Graph -> [Node]
nodes (Graph incs) = map fst incs

-- | Auxiliary function for descTree
descTree' :: Node -- the node to look for
          -> Graph -- the graph to look in
          -> [Node] -- ancestors to exclude
          -> RoseTree Node -- the tree of descendants
descTree' node graph ancs =
  RoseTree node
           (map (\n ->
                   descTree' n graph (node : ancs))
                nonAncestorIncidents)
  where allIncidents = incidents node graph
        nonAncestorIncidents = allIncidents \\ ancs

-- | Return a rosetree of descendants of a node.
descTree :: Node -> Graph -> RoseTree Node
descTree node graph = descTree' node graph []

-- | Return a pair containing the length of the longest path
-- | starting at a node, and the path itself.
longestPathFrom :: Node -> Graph -> (Depth, Path)
longestPathFrom node graph =
  let (depth',endNode,nodePath) =
        maxDepth' 0 (descTree node graph) []
  in (depth',nodePath ++ [endNode])
  where maxDepth' depth (RoseTree n lst) path =
          maximum $
          (depth + 1,n,path) :
          map (\tree ->
                 maxDepth' (depth + 1)
                           tree
                           (path ++ [n])) lst

-- | There may be multiple longest paths. Return them all in a list.
longestPaths :: Graph -> (Depth, [Path])
longestPaths graph = (depth, paths)
  where
    depth = fst $ head lst
    paths = map snd lst
    lst =
          maximums $
          map (`longestPathFrom` graph)
              (nodes graph)
          where maximums :: Ord a
                         => [(a,b)] -> [(a,b)]
                maximums =
                  head .
                  groupBy ((==) `on` fst) . sortBy (flip $ comparing fst)

-- | We delegate this work to three other functions.
isNeighbourOf :: String -> String -> Bool
isNeighbourOf s t =
  replacedN s t || addedN s t || removedN s t

addedN, removedN, replacedN
  :: String -> String -> Bool

-- | Try removing one character from the longer string and seeing
-- | if any choice of character makes them equal.
addedN p q =
  foldl1' (||) $
  map (\ix -> deleteNth ix p == q)
      [0 .. length p - 1]

removedN = flip addedN

-- | The strings should be different in only one position.
replacedN p q =
  length p == length q &&
  length (filter (uncurry (/=))
                 (zip p q)) == 1

-- | A small helper function.
deleteNth :: Int -> String -> String
deleteNth n xs = ys ++ tail'
  where (ys,zs) = splitAt n xs
        tail' =
          case zs of
            [] -> []
            _ -> tail zs

-- | Take a string and a list of strings and return the neighbours
-- | of the string.
neighbours :: String -> [String] -> [String]
neighbours str = filter (isNeighbourOf str)

-- | Generate the graph associated to a list of words.
generateGraph :: [String] -> Graph
generateGraph strs =
  Graph $ map (\s -> (s,neighbours s strs)) strs

-- | The program returns paths and the reversed paths separately,
-- | which is irritating.
removeReverses :: (Depth,[Path]) -> (Depth,[Path])
removeReverses (depth,paths) = (depth, paths')
  where paths' = map head $ groupBy (\x y -> x == y || x == reverse y) paths

-- | Et voilà!
main :: IO ()
main =
  do putStrLn "Enter words, separated by spaces:"
     wordList <- getLine
     let (pathlen,paths) = removeReverses .
                           longestPaths . generateGraph . words $
                           wordList
     putStrLn $ "The longest path length is " ++ show pathlen
     putStrLn "Paths:"
     mapM_ (putStrLn . intercalate " -> ") paths
