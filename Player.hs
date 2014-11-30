module Player where
import Board
import BoardUpdate
import Data.Maybe
import Control.Monad
import Human 
import AI

data Player = Human | RandomAI | LazyAI | HeuristicAI1 | HeuristicAI2 | MinimaxAI | MinimaxabAI deriving (Show, Read)

getStrategy :: Player -> (Board, Status) -> (Board, Status)
getStrategy Human = human
getStrategy RandomAI = randomAI
getStrategy LazyAI = lazyAI
getStrategy HeuristicAI1 = heuristicAI1
getStrategy HeuristicAI2 = heuristicAI2
getStrategy MinimaxAI = minimaxAI
getStrategy MinimaxabAI = minimaxabAI

playerList = [Human, RandomAI, LazyAI, HeuristicAI1, HeuristicAI2, MinimaxAI, MinimaxabAI]
gameOptions = [player | player <- playerList]



