module Game where
import Board
import BoardUpdate
import Player

playWith :: (Board, Status, (Player, Player)) -> (Board, Status,  (Player, Player))
playWith (board, status, (current, next)) = (newBoard, newStatus, (next, current)) where
                                                boardTuple = getStrategy current (board, status)
                                                newBoard = fst boardTuple
                                                newStatus = snd boardTuple
                                                
playGameWith p1 p2 = until (gameIsWon) playWith (emptyBoard, X, (p1, p2))

fullGameWith p1 p2 = putStrLn (winnerString (playGameWith p1 p2))

winnerString (board, status, (player1, player2))
  | declareDraw board = "This game is a Draw"
  | otherwise = ("\n" ++ show (nextStatus status)) ++ " is the winner!!!!\nGood Job, " ++ (show player2) ++ "!"

gameIsWon (board, status, (player1, player2)) = winnerExists board

drawGame (board, status, (player1, player2)) = declareDraw board

getResult (board, status, (player1, player2)) = (board, status)

trainClassifier :: Int -> [(Board, Status)] -> IO()
trainClassifier 0 completedTrials = writeFile "TrainingData" (show completedTrials)
trainClassifier n completedTrials = do
    let trial = getResult (playGameWith RandomAI RandomAI)
    let newCompletedTrials = trial : completedTrials
    trainClassifier (n-1) newCompletedTrials
    

