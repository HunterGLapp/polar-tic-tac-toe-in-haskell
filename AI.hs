module AI where
import Board
import BoardUpdate
import Data.Maybe(fromJust)
import System.IO.Unsafe
import System.Random
import Control.Monad
import GameTree
import Minimax

lazyAI' :: (Board, Status) ->IO (Board, Status)
lazyAI' (board, status) = do
  putStrLn("\nlazyAI's turn\n")
  putBoard (fst newBoard)
  return newBoard
    where
      move
        |isEmpty board = (0,0)
        |otherwise = head (getAvailableMoves board)
      newBoard = (fromJust (setC move status board), nextStatus status)

lazyAI = unsafePerformIO . lazyAI'



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





heuristicAI' :: (Board -> Status -> Int) -> (Board, Status) -> IO (Board, Status)
heuristicAI' heuristic (board, status) = do
  putStrLn("\nheuristicAI's turn\n")
  putBoard (fst newBoard)
  return newBoard
    where
      move
        |isEmpty board = (2, 2)
        |otherwise = getBestMove heuristic (board, status)
      newBoard = (fromJust (setC move status board), nextStatus status)

heuristicAI1 = unsafePerformIO . (heuristicAI' heuristic1)

heuristicAI2 = unsafePerformIO . (heuristicAI' heuristic2)

minimaxAI' :: (Board, Status) -> IO (Board, Status)
minimaxAI' (board, status) = do
  putStrLn("\nminimaxAI's turn\n")
  putBoard (fst newBoard)
  return newBoard
    where
      move
        |isEmpty board = (2, 2)
        |otherwise = getBestMoveMM (ticTacToeTree (board, status))
      newBoard = (fromJust (setC move status board), nextStatus status)

minimaxAI = unsafePerformIO . minimaxAI'

minimaxabAI' :: (Board, Status) -> IO (Board, Status)
minimaxabAI' (board, status) = do
  putStrLn("\nminimaxabAI's turn\n")
  putBoard (fst newBoard)
  return newBoard
    where
      move
        |isEmpty board = (2, 2)
        |otherwise = getBestMoveMMab (ticTacToeTree (board, status))
      newBoard = (fromJust (setC move status board), nextStatus status)

minimaxabAI = unsafePerformIO . minimaxabAI'
