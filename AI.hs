module AI where
import Board
import BoardUpdate
import Data.Maybe(fromJust)
import System.IO.Unsafe

randomAI' :: (Board, Status) ->IO (Board, Status)
randomAI' (board, status) = do
  putStrLn("\nrandomAI's turn\n")
  putBoard (fst newBoard)
  return newBoard
    where
      move
        |isEmpty board = (0, 0)
        |otherwise = head (getAvailableMoves board)
      newBoard = (fromJust (setC move status board), nextStatus status)

randomAI = unsafePerformIO . randomAI'

getAvailableMoves :: Board -> [(Int, Int)]
getAvailableMoves board = filter (validMove board) indices

validMove :: Board -> (Int, Int)  -> Bool
validMove board pos = validPosBool pos && fromJust (getC pos board) == Empty && hasNeighbors pos board

