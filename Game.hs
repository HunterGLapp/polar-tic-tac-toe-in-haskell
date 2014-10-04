module Game where
import Board
import BoardUpdate
import Player

playWith :: (Board, Status, (Player, Player)) -> (Board, Status,  (Player, Player))
playWith (board, status, (current, next)) = (newBoard, newStatus, (next, current)) where
                                                boardTuple = getStrategy current (board, status)
                                                newBoard = fst boardTuple
                                                newStatus = snd boardTuple
                                                
playGameWith p1 p2 = until gameIsWon playWith (emptyBoard, X, (p1, p2))

fullGameWith p1 p2 = putStrLn (winnerString (playGameWith p1 p2))

winnerString (board, status, (player1, player2)) = (show (nextStatus status)) ++ " is the winner!!!!"

gameIsWon (board, status, (player1, player2)) = winnerExists board

humanGame = fullGameWith Human Human
