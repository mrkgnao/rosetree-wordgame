module RTreeWordGame where

import Data.Maybe (fromJust)
import Data.List (lookup, (\\), groupBy, sortBy, foldl1', intercalate)
import Data.Function (on)
import Data.Ord (comparing)

type Node = String
type Depth = Int

type IncPair = (Node, [Node])
data Graph =
  Graph [IncPair]
  deriving (Show,Eq)

data RoseTree a =
  RoseTree a [RoseTree a]
  deriving (Show,Eq)

incidents :: Node -> Graph -> Maybe [Node]
incidents node (Graph incs) = lookup node incs

nodes :: Graph -> [Node]
nodes (Graph incs) = map fst incs

-- | Print a tree of descendants of a node.
descTree' :: Node -- the node to look for
          -> Graph -- the graph to look in
          -> [Node] -- ancestors to exclude
          -> RoseTree Node -- the tree of descendants

descTree' node graph ancs =
  RoseTree node
           (map (\n ->
                   descTree' n
                             graph
                             (node : ancs))
                 nonAncestorIncidents)
    where allIncidents = fromJust $ incidents node graph
          nonAncestorIncidents = allIncidents \\ ancs

descTree :: Node -> Graph -> RoseTree Node
descTree node graph = descTree' node graph []

longestPathFrom :: Node -> Graph -> (Depth, [Node])
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
                           (path ++ [n]))
              lst

longestPaths :: Graph -> [(Depth, [Node])]
longestPaths graph =
  maximums $
  map (\n -> longestPathFrom n graph)
      (nodes graph)
  where maximums :: Ord a
                 => [(a,b)] -> [(a,b)]
        maximums =
          head . groupBy ((==) `on` fst) . sortBy (flip $ comparing fst)

isNeighbourOf :: String -> String -> Bool
isNeighbourOf s t =
  replacedN s t || addedN s t || removedN s t

addedN, removedN, replacedN
  :: String -> String -> Bool
addedN p q =
  foldl1' (||) $
  map ((== q) . (\ix -> deleteNth ix p))
      [0 .. length p - 1]
removedN = flip addedN
replacedN p q =
  length (p' \\ q') == 1 && length (q' \\ p') == 1
  where [p',q'] =
          map (zip [0 ..])
              [p,q]

deleteNth :: Int -> String -> String
deleteNth n xs = ys ++ tail'
  where (ys,zs) = splitAt n xs
        tail' =
          case zs of
            [] -> []
            _ -> tail zs

neighbours :: String -> [String] -> [String]
neighbours str = filter (isNeighbourOf str)

generateGraph :: [String] -> Graph
generateGraph strs = Graph $ map (\s -> (s, neighbours s strs)) strs

removeReverses :: (Eq b) => [(a,[b])] -> [(a,[b])]
removeReverses = map head . groupBy (\(_,x) (_,y) -> x == y || x == reverse y)

main :: IO ()
main = do
  putStrLn "Enter words, separated by spaces:"
  wordList' <- getLine
  let wordList = words wordList'
      paths' = removeReverses $ longestPaths (generateGraph wordList)
      pathlen = fst . head $ paths'
      paths = map snd paths'
  putStrLn $ "The longest path length is " ++ show pathlen
  putStrLn "Paths:"
  mapM_ (putStrLn . intercalate " -> ") paths
