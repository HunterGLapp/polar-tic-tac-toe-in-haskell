module Player where
import Board
import BoardUpdate
import Data.Maybe
import Control.Monad
import Human 
import AI

data Player = Human | RandomAI | LazyAI | HeuristicAI deriving (Show, Read)

getStrategy :: Player -> (Board, Status) -> (Board, Status)
getStrategy Human = human
getStrategy RandomAI = randomAI
getStrategy LazyAI = lazyAI
getStrategy HeuristicAI = heuristicAI

playerList = [Human, RandomAI, LazyAI, HeuristicAI]
gameOptions = [player | player <- playerList]



