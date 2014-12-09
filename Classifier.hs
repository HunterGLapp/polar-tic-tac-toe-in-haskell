module Classifier where

import Board
import System.Environment
import Control.Monad
import Data.List
import System.IO.Unsafe

data BoardPrediction = Win | Loss
                     deriving(Show, Eq)
                             
type Probability = Double

argmax :: (Ord n) => (a -> n) -> [a] -> [a]
argmax fn set = [x | x <- set, fn x >= maximum (map fn set)]

argmax1 :: (Ord n) => (a -> n) -> [a] -> a
argmax1 fn set = head (argmax fn set)

classes = [Win, Loss]

prior Win = 0.5
prior Loss = 0.5

classify :: (Board, Status) -> BoardPrediction
classify (board, status) = argmax1 (p_func2 (board, status)) classes

classifyHeuristic :: Board -> Status -> Int
classifyHeuristic board status = evaluate (classify (board, status)) where
  evaluate Win = 1
  evaluate Loss = -1

p_func :: (Board, Status) -> BoardPrediction -> Double
p_func  (board, status) c = (prior c) * (product (getProbList c (board, status)))

readData = do
  input <- readFile "TrainingData"
  let boardArr = read input :: [(Board, Status)]
  return boardArr

getProbList' boardPred currState  = do
  input <- readData
  let l = toEnum (length input)
  let counts = map (getCounts boardPred currState) input
  let countArrs =  map sum (transpose counts)
  let probArrs = map ( * (1 / l)) countArrs
  return probArrs

getProbList boardPred currState = unsafePerformIO (getProbList' boardPred currState)

-- takes a board prediction and the current Board and a finished game, and returns
-- a list of ints representing whether or not the status observed in each space
-- is equal to the status in the finished game if the prediction is correct
-- or list of zeroes if the prediction is incorrect
  
getCounts :: BoardPrediction -> (Board, Status) -> (Board, Status) -> [Double]
getCounts pred (currBoard, myStatus) (board, status) 
  | ((status == nextStatus myStatus) && (pred == Win)) = compareBoards currBoard board
  | ((status == myStatus) && (pred == Loss)) = compareBoards currBoard board
  | otherwise = zBoard
  where
    zBoard = replicate 48 0
    compareBoards b1 b2 = map boolToInt (zipWith (==) (concat b1) (concat b2)) where
      boolToInt True = 1
      boolToInt False = 0
        

getProbLists boardPred status = [getProbList boardPred (emptyBoard, status),
                                getProbList boardPred (allXs, status),
                                getProbList boardPred (allOs, status)] where
                                  allXs = replicate 4 (replicate 12 X)
                                  allOs = replicate 4 (replicate 12 O)
                                  
winProbs status = getProbLists Win status
lossProbs status = getProbLists Loss status
  
p_func2 :: (Board, Status) -> BoardPrediction -> Double
p_func2 (board, status) c
  |c == Win = wp
  |c == Loss = lp
  where
    wp = getProb (winProbs (nextStatus status)) board
    lp = getProb (lossProbs (nextStatus status)) board
    
getProb sProbs board = e * x * o
  where
    e = product (zipWith (*) (sProbs !! 0) (only Empty board))
    x = product (zipWith (*) (sProbs !! 1) (only X board))
    o = product (zipWith (*) (sProbs !! 2) (only O board))
              
only status board = map (isSame status) (concat board)
            
isSame status1 status2
  |status1 == status2 = 1
  |otherwise = 0
