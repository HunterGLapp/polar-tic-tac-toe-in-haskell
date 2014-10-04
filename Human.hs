module Human (human) where
import Board
import BoardUpdate
import Data.Maybe
import Control.Monad
import System.IO.Unsafe


getNextMoveHuman :: (Board, Status) -> IO (Board, Status)
getNextMoveHuman (board, status) = do
  putStrLn ("\n" ++ (show status))
  move <- getCoords board
  let newBoard = (fromJust (setC move status board), nextStatus status)
  putBoard (fst newBoard)
  return newBoard

human = unsafePerformIO . getNextMoveHuman

getCoords :: Board -> IO (Int, Int)
getCoords board = do
  putStrLn ("Please enter some valid coordinates in the form (row, column) keeping in mind zero indexing is used")
  input <- getLine
  case readMaybe input of
    Just validInput ->
        if (validPosBool validInput) && ( fromJust (getC validInput board) == Empty) then (return validInput)
           else (putStrLn "not a valid move" >> getCoords board)
    Nothing -> putStrLn "Invalid coordinates" >> getCoords board

readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing
