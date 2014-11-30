module Classifier where

import Board
import System.Environment
import Control.Monad

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
classify (board, status) = argmax1 (p_func (board, status)) classes 

p_func :: (Board, Status) -> BoardPrediction -> Double
p_func  (board, status) c = (prior c) * (product (getProbList c (board, status)))

getProbList :: BoardPrediction -> (Board, Status) -> [Double]
getProbList Win (board, status) = replicate 48 0.5
getProbList Loss (board, status) = map (1 -) (getProbList Win (board, status))



buildData = do
  input <- readFile "TrainingData"
  let boardArr = read input :: (Board, Status)
  return boardArr

