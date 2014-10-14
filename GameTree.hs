module GameTree where
import AI
import Board
import BoardUpdate
import Player

data RoseTree a = Node a [RoseTree a] deriving (Show, Eq, Ord)

instance Functor RoseTree where
  fmap f (Node v children) = (Node (f v) (map (fmap f) children))

ticTacToeTree (board, status) = Node (board, status) (map ticTacToeTree (possibleNewStates board status))

getRoot :: RoseTree a -> a
getRoot (Node root _) = root

getChildren :: RoseTree a -> [RoseTree a]
getChildren (Node _ children) = children

trunc :: Int -> RoseTree a -> RoseTree a
trunc 0 x = Node (getRoot x) []
trunc n rosetree = Node (getRoot rosetree) (map (trunc (n - 1)) (getChildren rosetree))

labelWithHeuristic myStatus (board, status) = (board, status, heuristic board myStatus)

labelWithHeuristics status rosetree = fmap (labelWithHeuristic status) rosetree


