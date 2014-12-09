module TDNN where
import Board
import BoardUpdate
import GameTree

import Data.List
import Data.List.Split
import System.Random
import System.IO.Unsafe

type Weight = Double
type Neuron = [Weight]
type Layer = [Neuron]
type Net = [Layer]

activationFn x = tanh x
activationFn' x = (1/ (cosh x)) ^2

getNeuronOutput :: [Double] -> Neuron -> Double
getNeuronOutput inputs weights = activationFn(sum (zipWith (*) inputs weights))

getLayerOutputs:: [Double] -> Layer -> [Double]
getLayerOutputs inputs layer = map (getNeuronOutput inputs) layer

--Passes inputs through network and returns outputs
runThroughNet :: [Double] -> Net -> [Double]
runThroughNet inputs net = foldl' getLayerOutputs inputs net

--Runs a board through the network and returns output
runBoard :: Board -> Net -> Double
runBoard board net = head (runThroughNet (getInputs board) net)


--Initialize the network with small random weights
--takes number of layers and list of neurons per layer
--as parameters

initializeNetwork :: Int -> [Int] -> IO Net
initializeNetwork inputs neuronsPerLevel = do
  weights_inf <-  randomWeights
  let weights = take randomsNeeded weights_inf
  let div1 = chunksOf inputs weights
  let div2 = splitPlaces neuronsPerLevel div1
  return div2
  where
    randomsNeeded = sum (map (* inputs) neuronsPerLevel)

randomWeights :: IO [Double]
randomWeights = do
                gen <- getStdGen
                let weights = randomRs (0, 0.2) gen
                return weights

--Converts a board to a list of weights
getInputs :: Board -> [Double]
getInputs board = map statusToNum (concat board) where
  statusToNum status
    |status == X     = 1
    |status == O     = -1
    |status == Empty = 0

--  theta(s_t + 1) = theta(s_t) + alpha * (R(s_t+1) + gamma * V(s_t+1) - V(s_t)) * e_t
--Theta is the weight vector, e is an eligibility trace vector
getNetUpdate :: Net -> Board -> Board -> Double -> Double -> Double
getNetUpdate theta s s' alpha gamma = (alpha *
                                         (
                                           (r s') +
                                           gamma *
                                           (v s') - (v s)))
  where
    r state
      | winnerExists s' = 1
      | otherwise = 0
    v state = runBoard state theta

getEUpdate :: [Double] -> (Int, Int) -> Double -> Double -> [Double]
getEUpdate last_e move lambda gamma = replace_n movePos incremented (scaled)
  where
    scaled = map (* (lambda * gamma)) last_e
    replace_n n newVal (x:xs)
      | n == 0 = newVal:xs
      | otherwise = x:replace_n (n-1) newVal xs
    incremented = (last_e !! movePos) + 1
    movePos = intPos move
      where
        intPos (x1, x2) = 12 * x1 + x2 

readNet :: IO Net
readNet = do
  input <- readFile "TDNN_weights"
  let net = read input :: Net
  return net

writeNet :: Net -> IO()
writeNet net = do
  writeFile "TDNN_weights" (show net)

readETrace :: IO [Double]
readETrace = do
  input <- readFile "TDNN_ETrace"
  let eTrace = read input :: [Double]
  return eTrace

writeETrace :: [Double] -> IO()
writeETrace trace = do
  writeFile "TDNN_ETrace" (show trace)

getBestMoveTDNN :: Board -> Status -> Net-> (Int, Int)
getBestMoveTDNN board status net = (getAvailableMoves board) !!
                                   (indexOfMax
                                   (map ($net)
                                   (map runBoard succs)))
  where
    succs = possibleNewBoards board status
