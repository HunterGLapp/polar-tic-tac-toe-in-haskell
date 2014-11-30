module TDNN where
import Board
import Data.List

type Weight = Double
type Net = [[[Weight]]]

activationFn x = tanh x
activationFn' x = (1/ (cosh x)) ^2
