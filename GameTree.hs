module GameTree where
import AI
import Board
import BoardUpdate
import Player
import Data.Tree


buildTree =
  unfoldTree (heuristicAndChildren)

heuristicAndChildren (board, status)
  |isWinner board status = (10, [])
  |isWinner board (nextStatus status) = (-10, [])
  |otherwise = (heuristic board status, possibleNewStates board status)
