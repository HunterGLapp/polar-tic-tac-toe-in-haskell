module GameTree where
import Board
import BoardUpdate
import Data.Maybe
import Data.List

data RoseTree a = Node a [RoseTree a] deriving (Show, Eq, Ord)

instance Functor RoseTree where
  fmap f (Node v children) = (Node (f v) (map (fmap f) children))

ticTacToeTree (board, status) = Node (board, status)
                                (map ticTacToeTree (possibleNewStates board status))

getRoot :: RoseTree a -> a
getRoot (Node root _) = root

getChildren :: RoseTree a -> [RoseTree a]
getChildren (Node _ children) = children

trunc :: Int -> RoseTree a -> RoseTree a
trunc 0 x = Node (getRoot x) []
trunc n rosetree = Node (getRoot rosetree)
                   (map (trunc (n - 1)) (getChildren rosetree))

labelWithHeuristic :: (Board -> Status -> Int) ->  Status -> (Board, Status) -> (Board, Status, Int)
labelWithHeuristic heuristic myStatus (board, status) = (board, status, heuristic board myStatus)

labelWithHeuristics :: (Board -> Status -> Int) -> Status -> RoseTree (Board, Status) -> RoseTree (Board, Status, Int)
labelWithHeuristics heuristic status roseTree
  |isLeaf roseTree = Node ((labelWithHeuristic heuristic status) (getRoot roseTree)) []
  |otherwise  = (Node (labelWithZero (getRoot roseTree))
                 ((map (labelWithHeuristics heuristic status) (getChildren roseTree)))) where
    labelWithZero (board, status) = (board, status, 0)
    isLeaf (myTree) = (getChildren (myTree) == [])



getHeuristic :: (Board, Status, Int) -> Int
getHeuristic (board, status, heuristic) = heuristic

sumHeuristics :: RoseTree (Board, Status, Int) -> Int
sumHeuristics rosetree = getHeuristic (getRoot rosetree) +
                         sum (map sumHeuristics (getChildren rosetree))

getBestMove :: Int -> (Board -> Status -> Int) -> (Board, Status) -> (Int, Int)
getBestMove n heuristic (board, status) = (getAvailableMoves board) !! (indexOfMax (childVals)) where
  childVals = map sumHeuristics childList where
    childList = getChildren (labelWithHeuristics heuristic status boardTree) where
      boardTree = trunc n (ticTacToeTree (board, status))

indexOfMax :: Ord a => [a] -> Int
indexOfMax list = head (elemIndices (maximum list) list)

possibleNewStates :: Board -> Status -> [(Board, Status)]
possibleNewStates board status = [(b, nextStatus status) | b <- possibleNewBoards board status]

possibleNewBoards :: Board -> Status -> [Board]
possibleNewBoards board status = map fromJust
                                 (map ($ board)
                                 (map ($ status)
                                  (map setC moves)))
                                 where moves = getAvailableMoves board

newMovesWithStatus board status = map (\x -> (fst x, snd x, status)) (newMoves board status)
  
showPossibilities :: [Board] -> String
showPossibilities boards =concat (map (++ "\n")
                                  (map (showBoard) boards))

putPossibilities = putStrLn . showPossibilities

newMoves :: Board -> Status -> [((Int, Int), Board)]
newMoves board status =  (zipWith (,) (getAvailableMoves board)
                          (possibleNewBoards board status))

getAvailableMoves :: Board -> [(Int, Int)]
getAvailableMoves board
  |isEmpty board = indices
  |otherwise = filter (validMove board) indices

validMove :: Board -> (Int, Int)  -> Bool
validMove board pos = validPosBool pos &&
                      fromJust (getC pos board) == Empty &&
                      hasNeighbors pos board

maxNeighbors :: Board -> Status -> Int
maxNeighbors board status = maximum (map (numNeighbors status)
                                     (allNeighbors board))

allNeighbors :: Board -> [[[Maybe Status]]]
allNeighbors board = map ($board)
                     (map getNeighbors indices)

numNeighbors :: Status -> [[Maybe Status]] -> Int
numNeighbors status maybeNeighbors = sum (map (countable status)
                                          (concat maybeNeighbors))

countable :: Status -> Maybe Status -> Int
countable status maybeStatus
  |maybeStatus == Nothing = 0
  |fromJust maybeStatus == status = 1
  |otherwise = 0

heuristic1 :: Board -> Status -> Int
heuristic1 board status
  |isWinner board status = 100
  |isWinner board (nextStatus status) = -100
  |otherwise = (maxNeighbors board status) - (maxNeighbors board (nextStatus status))

heuristic2 :: Board -> Status -> Int
heuristic2 board status
  |isWinner board status = 100
  |isWinner board (nextStatus status) = -200
  |otherwise = 0
