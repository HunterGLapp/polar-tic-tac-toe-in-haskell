module Player where
import Board
import BoardUpdate
import Data.Maybe
import Control.Monad
import Human
import AI

data Player = Human | RandomAI | LazyAI deriving (Show, Read)

getStrategy :: Player -> (Board, Status) -> (Board, Status)
getStrategy Human = human
getStrategy RandomAI = randomAI
getStrategy LazyAI = lazyAI

playerList = [Human, RandomAI, LazyAI]
gameOptions = [(player1, player2) | player1 <- playerList, player2 <- playerList]



