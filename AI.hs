module AI where
import Board
import BoardUpdate
import Data.Maybe(fromJust)
import System.IO.Unsafe
import System.Random
import Control.Monad

lazyAI' :: (Board, Status) ->IO (Board, Status)
lazyAI' (board, status) = do
  putStrLn("\nlazyAI's turn\n")
  putBoard (fst newBoard)
  return newBoard
    where
      move
        |isEmpty board = (0,0)
        |otherwise = head(getAvailableMoves board)
      newBoard = (fromJust (setC move status board), nextStatus status)

lazyAI = unsafePerformIO . lazyAI'

getAvailableMoves :: Board -> [(Int, Int)]
getAvailableMoves board = filter (validMove board) indices

validMove :: Board -> (Int, Int)  -> Bool
validMove board pos = validPosBool pos && fromJust (getC pos board) == Empty && hasNeighbors pos board

randomAI' (board, status) = do
  putStrLn "\nRandom AI's turn\n"
  posCandidate <- randomPos
  let newBoard = ((fromJust (setC posCandidate status board)), nextStatus status)
  if (fromJust (getC posCandidate board)  == Empty && (hasNeighbors posCandidate board)) || isEmpty board then putBoard (fst newBoard) >> return newBoard else randomAI' (board, status)


randomAI = unsafePerformIO . randomAI'

randomIndex = getStdRandom(randomR (0, 47))

randomPos = do
            index <- randomIndex
            return (indices !! index)
            

