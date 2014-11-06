import Game
import Board
import BoardUpdate
import Player
import Human
import AI

main :: IO()
main = do
  players <- getChoice
  fullGameWith (fst players) (snd players)

getChoice :: IO(Player, Player)
getChoice = do
  putStrLn("\nPlease choose two of the following options and imput them as a tuple, i.e. (player1, player2).\nAdditional options will be displayed if relevant.\n")
  putStrLn (show gameOptions)
  choice <- getLine
  case readMaybe choice of
    Just valid ->
      return valid
    Nothing ->
      putStrLn "\n\nSorry, that is not valid input, Try again?" >> getChoice
  
    
