--Main file for Polar Tic Tac Toe (PTT)
--Hunter Lapp
--Artificial Intelligence Fall 2014

module Board (Status(X, O, Empty),
              Row,
              Board,
              putBoard,
              putMaybeBoard,
              emptyBoard,
              showMaybeStatus,
              nextStatus) where
import Data.Maybe
import Control.Monad

data Status  = X | O | Empty  deriving (Eq)
instance Show Status where
  show X = "X"
  show O = "O"
  show Empty = "_"
type Row = [Status]
type Board = [Row]

nextStatus :: Status -> Status
nextStatus X = O
nextStatus O = X

--Printing functions

showBoard :: Board -> String
showBoard board = unlines (map show board)

putBoard :: Board -> IO()
putBoard = putStr . showBoard

showMaybeBoard :: Maybe Board -> String
showMaybeBoard Nothing = "Not a valid move"
showMaybeBoard (Just board) = showBoard board

putMaybeBoard :: Maybe Board -> IO()
putMaybeBoard = putStr . showMaybeBoard

showMaybeStatus :: Maybe Status -> String
showMaybeStatus maybeStatus
  |isNothing maybeStatus = "N"
  |otherwise = show (fromJust maybeStatus) 
  
emptyBoard = replicate 4 (replicate 12 Empty) :: Board
