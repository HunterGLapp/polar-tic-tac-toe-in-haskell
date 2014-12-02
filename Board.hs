--Polar Tic Tac Toe
--Hunter Lapp
--Artificial Intelligence Fall 2014

module Board (Status(X, O, Empty),
              Row,
              Board,
              putBoard,
              putMaybeBoard,
              emptyBoard,
              showMaybeStatus,
              nextStatus,
              showBoard,
              isEmpty) where
import Data.Maybe
import Control.Monad

--Representing the status of a board space
data Status  = X | O | Empty  deriving (Eq, Read, Show)

--Pretty Printing for statuses for displaying board
pprint X = "  X"
pprint O = "  O"
pprint Empty = "  _"

type Row = [Status]
type Board = [Row]

--Returns the other status
nextStatus :: Status -> Status
nextStatus X = O
nextStatus O = X

--Converts a board to a string
showBoard :: Board -> String
showBoard board = "  0  1  2  3  4  5  6  7  8  9 10 11\n" ++
                  "_____________________________________\n" ++
                  concat (map showRow board) where
                    showRow row = concat (map pprint row) ++ "\n"

--Prints a board
putBoard :: Board -> IO()
putBoard = putStr . showBoard

--Used for printing board, or returning an error string if invalid move
--has been made
showMaybeBoard :: Maybe Board -> String
showMaybeBoard Nothing = "Not a valid move"
showMaybeBoard (Just board) = showBoard board

putMaybeBoard :: Maybe Board -> IO()
putMaybeBoard = putStr . showMaybeBoard

showMaybeStatus :: Maybe Status -> String
showMaybeStatus maybeStatus
  |isNothing maybeStatus = "N"
  |otherwise = show (fromJust maybeStatus) 

--The starting board. all spaces are empty
emptyBoard = replicate 4 (replicate 12 Empty) :: Board

isEmpty board = all (== Empty) (concat board)
