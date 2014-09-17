module Game where

import Board
import BoardUpdate
import Player
import Data.Maybe

data Game = Finished Board | Unfinished Board

-- a function that should absolutely be in prelude but isn't

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing
              
--game :: Game -> IO ()
game = do
  let oldBoard = emptyBoard
  let xPlayer = X
  putStrLn "Player 1 (X's)"
  xMove <- getCoords
  let newBoardAfterX = setC xMove xPlayer oldBoard
  putMaybeBoard newBoardAfterX
  let oPlayer = O
  putStrLn "Player 2 (O's)"
  oMove <- getCoords
  let newBoardAfterX' = fromJust newBoardAfterX
  let newBoardAfterO = setC oMove oPlayer newBoardAfterX'
  putMaybeBoard newBoardAfterO
  return newBoardAfterO

getCoords :: IO (Int, Int)
getCoords = do
  putStrLn ("Please enter some valid coordinates")
  input <- getLine
  case readMaybe input of
    Just validInput ->
        if (validPosBool validInput) then (return validInput)
           else (putStrLn "coords not on board" >> getCoords)
    Nothing -> putStrLn "Invalid coordinates" >> getCoords
  

  
