module Player where
import Board
import BoardUpdate
import Data.Maybe
import Control.Monad
import Human
import AI

data Player = Human | RandomAI deriving Show

getStrategy :: Player -> (Board, Status) -> (Board, Status)
getStrategy Human = human
getStrategy RandomAI = randomAI





