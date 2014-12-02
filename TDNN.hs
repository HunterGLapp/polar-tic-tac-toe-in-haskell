module TDNN where
import Board
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
runBoard :: Board -> Net -> [Double]
runBoard board net = runThroughNet (getInputs board) net

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

--  V(s_t) <- V(s_t) + alpha * (R(s_t+1) + gamma * V(s_t+1) - V(s_t))
getUpdate v s alpha r s' gamma = (v s) + (alpha * ((r s) + gamma * (v s') - (v s')))
