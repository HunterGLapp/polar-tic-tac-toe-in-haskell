module GameTree where
import AI
import Board
import BoardUpdate
import Player

data RoseTree a = Node a [RoseTree a] deriving (Show, Eq, Ord)

ticTacToeTree (board, status) = Node (board, status) (map ticTacToeTree (possibleNewStates board status))

