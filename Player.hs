module Player where
import Board
import BoardUpdate

data Player = Player1 | Player2

fromStatus :: Status -> Player
fromStatus X = Player1
fromStatus O = Player2
fromStatus Empty = error "fromStatus failed. Don't pass Emptys to this function"

getMove :: Player -> (Int, Int) -> Board -> Maybe Board
getMove Player1 (x, y) = setC (x, y) X 
getMove Player2 (x, y) = setC (x, y) O 
