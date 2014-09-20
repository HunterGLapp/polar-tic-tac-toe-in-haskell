module Game where

import Board
import BoardUpdate
import Player
import Data.Maybe
import Control.Monad
import System.IO.Unsafe

-- a function that should absolutely be in prelude but isn't

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

  
nextStatus :: Status -> Status
nextStatus X = O
nextStatus O = X


turn :: Board -> Status -> IO (Board)
turn board status = do
  putStrLn (show status)
  putStrLn "your turn"
  move <- getCoords board
  let newBoard = fromJust (setC move status board) 
  putBoard newBoard
  return newBoard

{-h (board, status) = until (sequence (evaluate winningBoardState)) (transition . sequenceTuple) (evaluate emptyBoard, evaluate X)
  where
    transition (board, status) = do
      oldBoard <- board
      oldStatus <- status
      putStrLn (show status)
      move <- getCoords
      return (
               (fromJust (setC move oldStatus oldBoard),
                nextStatus oldStatus))
    winningBoardState (board, status) = do
                                        myBoard <- board
                                        return winnerExists myBoard
-}

playGame = until gameIsWon unsafeGetNextMove (emptyBoard, X)

winnerString :: (Board, Status) -> String
winnerString (board, status) = (show (nextStatus status)) ++ " is the winner!"

fullGame = putStrLn (winnerString playGame)

getNextMove :: (Board, Status) -> IO (Board, Status)
getNextMove (board, status) = do
  putStrLn (show status)
  move <- getCoords board
  let newBoard = (fromJust (setC move status board), nextStatus status)
  putBoard (fst newBoard)
  return newBoard
  
unsafeGetNextMove = unsafePerformIO . getNextMove

gameIsWon :: (Board, Status) -> Bool
gameIsWon (board, status) = winnerExists board

sequenceTuple ::  IO  (a, b) ->  (IO a, IO b)
sequenceTuple tuple = (return (fst rawTuple), return (snd rawTuple))
  where rawTuple = unsafePerformIO tuple
        
getCoords :: Board -> IO (Int, Int)
getCoords board = do
  putStrLn ("Please enter some valid coordinates")
  input <- getLine
  case readMaybe input of
    Just validInput ->
        if (validPosBool validInput) && ( fromJust (getC validInput board) == Empty) then (return validInput)
           else (putStrLn "not a valid move" >> getCoords board)
    Nothing -> putStrLn "Invalid coordinates" >> getCoords board
  

  
