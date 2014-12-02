module Player where
import Board
import BoardUpdate
import Data.Maybe
import Control.Monad
import Human 
import AI
import Classifier

data Player = Human | RandomAI | LazyAI | HeuristicAI1 | HeuristicAI2 | MinimaxAI | MinimaxabAI | NaiveBayesAI deriving (Show, Read)

--Returs a strategy for each player
getStrategy :: Player -> (Board, Status) -> (Board, Status)
getStrategy Human = human
getStrategy RandomAI = randomAI
getStrategy LazyAI = lazyAI
getStrategy HeuristicAI1 = heuristicAI1
getStrategy HeuristicAI2 = heuristicAI2
getStrategy MinimaxAI = minimaxAI
getStrategy MinimaxabAI = minimaxabAI
getStrategy NaiveBayesAI = naiveBayesAI

--List of available player agents
playerList = [Human, RandomAI, LazyAI, HeuristicAI1, HeuristicAI2, MinimaxAI, MinimaxabAI, NaiveBayesAI]
gameOptions = [player | player <- playerList]



