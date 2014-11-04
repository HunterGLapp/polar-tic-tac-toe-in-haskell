module Classifier where
import Board

data BoardPrediction = Win | Loss
                     deriving(Show, Eq)
                             
type Probability = Double

argmax :: (Ord n) => (a -> n) -> [a] -> [a]
argmax fn set = [x | x <- set, fn x >= maximum (map fn set)

argmax1 :: (Ord n) => (a -> n) -> [a] -> a
argmax1 fn set = head (argmax fn set)

classes = [Win, Loss]

prior Win = 0.5
prior Loss = 0.5

--classify :: (Board, Status) -> BoardPrediction
--classify board = argmax1 classes (

p_func :: BoardPrediction -> (Board, Status) -> Double
p_func c (board, status) = (prior c) * product 
