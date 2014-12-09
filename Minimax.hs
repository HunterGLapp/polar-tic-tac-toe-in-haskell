module Minimax where
import GameTree
import Board
import Data.List

minimax :: RoseTree (Board, Status) -> Int
minimax (Node (board, status) []) = heuristic1 board status
minimax (Node (board, status) children) = maximum (map (negate . minimax) children)

truncMinimax rosetree = minimax (trunc 3 rosetree)

getBestMoveMM (Node (board, status) children) = (getAvailableMoves board) !! (indexOfMax (childVals)) where
  childVals = map negate (map truncMinimax children)
  
alphabeta rosetree = alphabeta' (negate score) (score) rosetree where
  score = heuristic1 (fst (getRoot rosetree)) (snd (getRoot rosetree))
  alphabeta' _ _ (Node (board, status) []) = heuristic1 board status
  alphabeta' alpha beta (Node _ children) = fst (foldl' getNew_ab (alpha, beta) children) where
    getNew_ab (alpha, beta) rosetree
      |alpha >= beta = (alpha, beta)
      |otherwise = (max alpha (negate (alphabeta' (negate beta) (negate alpha) rosetree)), beta)

truncab rosetree = alphabeta (trunc 3 rosetree)

getBestMoveMMab (Node (board, status) children) = (getAvailableMoves board) !! (indexOfMax (childVals)) where
  childVals = (map truncab children)
