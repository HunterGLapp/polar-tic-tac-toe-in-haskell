data BoardPrediction = Win | Loss
                     deriving(Show, Eq)

type Probability = Double

data Perhaps a = Perhaps a Probability

argmax f set = [x | x <- set, f x >= maximum (map f set)]

