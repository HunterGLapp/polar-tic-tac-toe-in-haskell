module Player where
import Board
import BoardUpdate
import Data.Maybe
import Control.Monad
import Human 
import AI

data Player = Human | RandomAI | LazyAI | HeuristicAI1 | HeuristicAI2 deriving (Show, Read)

getStrategy :: Player -> (Board, Status) -> (Board, Status)
getStrategy Human = human
getStrategy RandomAI = randomAI
getStrategy LazyAI = lazyAI
getStrategy HeuristicAI1 = heuristicAI1
getStrategy HeuristicAI2 = heuristicAI2

playerList = [Human, RandomAI, LazyAI, HeuristicAI1, HeuristicAI2]
gameOptions = [player | player <- playerList]



