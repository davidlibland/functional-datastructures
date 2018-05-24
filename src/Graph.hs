{-# LANGUAGE TemplateHaskell #-}

module Graph where
import Control.Lens
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as Map
-- import Data.Map ((!?))
import qualified Data.Set as Set
import SplayHeap
-- import Heap
import PriorityQueue

data Node a = Node a deriving (Show, Eq, Ord)
data Graph a = Graph {_nodes :: (Set.Set (Node a)), _edges :: (Map.Map (Node a)
[Node
 a])} deriving Show
makeLenses ''Graph

insertNode :: Ord a => a -> Graph a -> Graph a
insertNode x = nodes.(contains (Node x)).~ True

emptyGraph :: Graph a
emptyGraph = Graph {_nodes = Set.empty, _edges = Map.empty}

insertEdge :: Ord a => a -> a -> Graph a -> Graph a
insertEdge x y = (edges %~ (Map.insertWith (++) (Node x) [(Node y)]))
    . (insertNode x)
    . (insertNode y)

getOutgoing :: Ord a => Node a -> Graph a -> [Node a]
getOutgoing n = fromMaybe [] . view (edges . at n)

data DijStep a = DijStep {
    _pDist :: Map.Map (Node a) Int,
    _proxQueue :: SimpleHeapQueue SplayHeap (Node a) Int,
    _knownP :: Map.Map (Node a) (Node a),
    _visited :: Set.Set (Node a)
} deriving Show
makeLenses ''DijStep

getClosest :: Ord a => DijStep a -> Maybe (Node a)
getClosest = (fmap fst . findMin) . (view proxQueue)

deleteClosest :: Ord a => DijStep a -> Maybe (DijStep a)
deleteClosest = proxQueue deleteMin

popClosest :: Ord a => DijStep a -> Maybe (Node a, DijStep a)
popClosest s = (Just (,)) <*> (closest) <*> (rest)
    where
        closest = getClosest s
        rest = deleteClosest s

update :: Ord a => Graph a -> ((Node a), (DijStep a)) -> DijStep a
update g (x, s) = visited.(contains x).~ True $ s'
    where
        updateFrom y = if not closer
            then id
            else (pDist %~ (at y ?~ dy))
                . (proxQueue %~ (insert y dy))
                where
                (closer, dy) = case s^.pDist^.at y of
                    Nothing -> (True, dx + 1)
                    Just d -> if dx + 1 < d then (True, dx + 1) else (False, d)
        dx = fromJust $ s^.pDist^.at x
        s' = foldr updateFrom s $ getOutgoing x g

initStep :: Ord a => Node a -> DijStep a
initStep x = DijStep {
    _pDist = Map.empty & at x ?~ 0,
    _knownP = Map.empty,
    _proxQueue = insert x 0 empty,
    _visited = Set.empty
}

isVisited :: Ord a => Node a -> Maybe (DijStep a) -> Bool
isVisited y ms = case ms of
    Nothing -> True
    Just s -> s^.visited^.contains y

getDistance :: Ord a => [Maybe (DijStep a)] -> Node a -> Maybe Int
getDistance s y =  (>>= view (pDist . (at y))) $ head $ filter
    (isVisited y)  s

dijkstra :: Ord a => Graph a -> Node a -> Node a -> Maybe Int
dijkstra g x = getDistance searchStream
    where
        iterator = fmap (update g) . popClosest
        searchStream = iterate (>>= iterator) $ Just $ initStep x
